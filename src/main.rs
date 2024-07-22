// SPAGHETTI WARNING brace yourself

use std::collections::{BTreeMap, BTreeSet, VecDeque};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{Arc, Mutex, RwLock};

enum ConstProp<T> {
    Bottom,
    Val(T),
    Top,
}

impl<T> Default for ConstProp<T> {
    fn default() -> Self {
        ConstProp::Bottom
    }
}

impl<T: core::fmt::Debug> core::fmt::Debug for ConstProp<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Bottom => write!(f, "Bottom"),
            Self::Val(arg0) => f.debug_tuple("Val").field(arg0).finish(),
            Self::Top => write!(f, "Top"),
        }
    }
}

impl<T: PartialEq> PartialEq for ConstProp<T> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (ConstProp::Bottom, ConstProp::Bottom) => true,
            (ConstProp::Top, ConstProp::Top) => true,
            (ConstProp::Val(a), ConstProp::Val(b)) => a == b,
            _ => false,
        }
    }
}

impl<T: Clone> Clone for ConstProp<T> {
    fn clone(&self) -> Self {
        match self {
            Self::Bottom => Self::Bottom,
            Self::Val(arg0) => Self::Val(arg0.clone()),
            Self::Top => Self::Top,
        }
    }
}

impl<T: Copy> Copy for ConstProp<T> {}

impl<T: PartialOrd> PartialOrd for ConstProp<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (ConstProp::Top, _) => Some(std::cmp::Ordering::Greater),
            (_, ConstProp::Bottom) => Some(std::cmp::Ordering::Greater),
            _ => None,
        }
    }
}

trait Lattice: PartialOrd {
    fn bottom() -> Self;

    fn join(self, b: Self) -> Self;
}

impl Lattice for bool {
    fn bottom() -> Self {
        false
    }

    fn join(self, b: Self) -> Self {
        self || b
    }
}

impl Lattice for usize {
    fn bottom() -> Self {
        0
    }

    fn join(self, b: Self) -> Self {
        self.max(b)
    }
}

impl<T: PartialOrd> Lattice for ConstProp<T> {
    fn bottom() -> Self {
        ConstProp::Bottom
    }

    fn join(self, b: Self) -> Self {
        match (self, b) {
            (ConstProp::Bottom, b) => b,
            (ConstProp::Val(_), ConstProp::Top) => ConstProp::Top,
            (ConstProp::Val(_), ConstProp::Bottom) => ConstProp::Bottom,
            (ConstProp::Val(a), ConstProp::Val(b)) if a == b => ConstProp::Val(a),
            (ConstProp::Val(_), ConstProp::Val(_)) => ConstProp::Top,
            (ConstProp::Top, _) => ConstProp::Top,
        }
    }
}

impl<T: Ord + Eq, L: Lattice> Lattice for BTreeMap<T, L> {
    fn bottom() -> Self {
        BTreeMap::new()
    }

    fn join(self, b: Self) -> Self {
        self.into_iter().fold(b, |mut cur, (k, v)| {
            let l = cur.remove(&k).unwrap_or(L::bottom()).join(v);
            cur.insert(k, l);
            cur
        })
    }
}

struct ConcurrentLatticeElement<L: Lattice> {
    val: RwLock<L>,
}

impl<L: Lattice + core::fmt::Debug> core::fmt::Debug for ConcurrentLatticeElement<L> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ConcurrentLatticeElement")
            .field("val", &self.val)
            .finish()
    }
}

impl<L: Lattice> PartialEq for ConcurrentLatticeElement<L> {
    fn eq(&self, other: &Self) -> bool {
        self.val
            .read()
            .is_ok_and(|v| other.val.read().is_ok_and(|u| v.eq(&u)))
    }
}

impl<L: Lattice> PartialOrd for ConcurrentLatticeElement<L> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.val
            .read()
            .ok()
            .and_then(|v| other.val.read().ok().and_then(|u| v.partial_cmp(&u)))
    }
}

impl<L: Lattice> Lattice for ConcurrentLatticeElement<L> {
    fn bottom() -> Self {
        Self {
            val: RwLock::new(L::bottom()),
        }
    }

    fn join(self, b: Self) -> Self {
        Self {
            val: RwLock::new(
                self.val
                    .into_inner()
                    .unwrap()
                    .join(b.val.into_inner().unwrap()),
            ),
        }
    }
}

struct Worklist<T: Ord + Eq> {
    worklist_set: BTreeSet<Arc<T>>,
    worklist: VecDeque<Arc<T>>,
}

impl<T: Ord + Eq + core::fmt::Debug> core::fmt::Debug for Worklist<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Worklist")
            .field("worklist_set", &self.worklist_set)
            .field("worklist", &self.worklist)
            .finish()
    }
}

impl<T: Ord + Eq> Worklist<T> {
    fn new() -> Worklist<T> {
        Self {
            worklist: VecDeque::new(),
            worklist_set: BTreeSet::new(),
        }
    }

    fn push_back(&mut self, val: Arc<T>) {
        if !self.worklist_set.contains(&val) {
            self.worklist_set.insert(val.clone());
            self.worklist.push_back(val);
        }
    }

    fn pop_front(&mut self) -> Option<Arc<T>> {
        let val = self.worklist.pop_front()?;
        self.worklist_set.remove(&val);
        Some(val)
    }

    fn is_empty(&self) -> bool {
        self.worklist.is_empty()
    }
}

fn solve<
    T: Ord + Eq + Send + Sync + core::fmt::Debug,
    L: Lattice + Send + Sync + core::fmt::Debug,
>(
    initial: Vec<T>,
    transfer: fn(&T, L) -> L,
    join_pred: fn(Arc<RwLock<BTreeMap<Arc<T>, Arc<ConcurrentLatticeElement<L>>>>>, &T) -> L,
    threads: usize,
) -> BTreeMap<T, L> {
    let element = BTreeMap::new();
    let worklist = Mutex::new(Worklist::new());
    {
        let mut worklist = worklist.lock().unwrap();
        for t in initial {
            worklist.push_back(Arc::new(t));
        }
    }

    let working = AtomicUsize::new(0);
    let worklist = Arc::new(worklist);
    let element = Arc::new(RwLock::new(element));

    std::thread::scope(|s| {
        let threads = (0..threads)
            .map(|_| {
                s.spawn(|| {
                    let worklist = worklist.clone();
                    let element = element.clone();
                    loop {
                        let Some(next) = ({
                            let mut worklist = worklist.lock().unwrap();
                            let next = worklist.pop_front();
                            if worklist.is_empty() && working.load(Ordering::SeqCst) == 0 {
                                break;
                            }
                            next
                        }) else {
                            continue;
                        };
                        working.fetch_add(1, Ordering::SeqCst);

                        let joinpred = join_pred(element.clone(), &next);
                        let new = transfer(&next, joinpred);

                        let elem_entry = {
                            let lock = element.read().unwrap();
                            lock.get(&next).cloned()
                        }
                        .unwrap_or_else(|| {
                            let l = Arc::new(ConcurrentLatticeElement {
                                val: RwLock::new(L::bottom()),
                            });
                            element.write().unwrap().insert(next.clone(), l.clone());
                            l
                        });

                        if *elem_entry.val.read().unwrap() != new {
                            *elem_entry.val.write().unwrap() = new;
                            let mut worklist = worklist.lock().unwrap();
                            worklist.push_back(next);
                        }
                        working.fetch_sub(1, Ordering::SeqCst);
                    }
                })
            })
            .collect::<Vec<_>>();
        for thread in threads {
            thread.join().unwrap()
        }
    });

    assert!(worklist.lock().unwrap().is_empty());

    Arc::into_inner(element)
        .unwrap()
        .into_inner()
        .unwrap()
        .into_iter()
        .map(|(k, v)| {
            (
                Arc::into_inner(k).unwrap(),
                Arc::into_inner(v).unwrap().val.into_inner().unwrap(),
            )
        })
        .collect()
}

fn main() {
    // The join and transfer functions are for the following:
    // (initialize a, b, c = 0)
    // ```
    // if (b > 5) {
    //     c = b
    // } else {
    //     c = 5
    // }
    // a = c + 2
    // ```
    // sorry for the spaghetti mess :(
    let soln = solve(
        vec![0, 1, 2, 3],
        |a, mut b| -> BTreeMap<usize, ConstProp<usize>> {
            match a {
                0 => b,
                1 => {
                    *b.entry(2).or_default() = *b.get(&1).unwrap_or(&ConstProp::Bottom);
                    b
                }
                2 => {
                    *b.entry(2).or_default() = ConstProp::Val(5);
                    b
                }
                3 => {
                    *b.entry(0).or_default() = match b.get(&2) {
                        Some(ConstProp::Val(c)) => ConstProp::Val(c + 2),
                        Some(ConstProp::Top) => ConstProp::Top,
                        Some(ConstProp::Bottom) => ConstProp::Bottom,
                        None => ConstProp::Bottom,
                    };
                    b
                }
                _ => unreachable!(),
            }
        },
        |a, b| {
            let map = a.read().unwrap();
            match b {
                0 => BTreeMap::from_iter(
                    vec![
                        (0, ConstProp::Val(0)),
                        (1, ConstProp::Val(0)),
                        (2, ConstProp::Val(0)),
                    ]
                    .into_iter(),
                ),
                1 | 2 => match map.get(&0) {
                    Some(v) => v.val.read().unwrap().clone(),
                    None => BTreeMap::new(),
                },
                3 => {
                    let l: BTreeMap<_, _> = match map.get(&1) {
                        Some(v) => v.val.read().unwrap().clone(),
                        None => BTreeMap::new(),
                    };
                    let r: BTreeMap<_, _> = match map.get(&2) {
                        Some(v) => v.val.read().unwrap().clone(),
                        None => BTreeMap::new(),
                    };
                    l.join(r)
                }
                _ => unreachable!(),
            }
        },
        4,
    );

    println!("{soln:?}");
}
