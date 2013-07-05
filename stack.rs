struct Node<T> {
    elem: T,
    next: Option<@mut Node<T>>
}

struct Stack<T> {
    size: uint,
    head: Option<@mut Node<T>>,
}

impl<T> BaseIter<T> for Stack<T> {
    fn each(&self, op: &fn(v: &T) -> bool) -> bool {
        let mut iter = self.head;
        while iter.is_some() {
            let node = iter.get();
            let frozen_node = &*node;
            if !op(&frozen_node.elem) {
                return false;
            }
            iter = node.next;
        }
        return true;
    }

    fn size_hint(&self) -> Option<uint> {
        Some(self.size)
    }
}

pub fn push<T>(st: &Stack<T>, x: T) -> Stack<T> {
    Stack { size: st.size + 1, head: Some(@mut Node { elem: x, next: st.head }) }
}

impl<T> Stack<T> {
    pub fn new() -> Stack<T> {
        Stack { size: 0, head: None }
    }

    pub fn each_mut(&mut self, op: &fn(&mut T) -> bool) -> bool {
        let mut iter = self.head;
        while iter.is_some() {
            let node = iter.get();
            if !op(&mut node.elem) {
                return false;
            }
            iter = node.next;
        }
        return true;
    }
}

#[test]
fn each_test() {
    let tail: Stack<int> = Stack::new();
    let stack = push(&tail, 1);

    let mut v:~[int] = ~[];

    for stack.each |&i| {
        v.push(i);
    }

    assert_eq!(v, ~[1]);
}

#[test]
fn each_mut_test() {
    let tail: Stack<int> = Stack::new();
    let mut stack = push(&tail, 1);

    let mut v:~[int] = ~[];

    for stack.each_mut |i| {
        *i += 1;
    }

    for stack.each |&i| {
        v.push(i);
    }

    assert_eq!(v, ~[2]);
}
