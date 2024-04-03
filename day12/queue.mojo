@always_inline
fn _max(a: Int, b: Int) -> Int:
    return a if a > b else b


struct Queue[T: CollectionElement](Movable, Sized):
    var data: AnyPointer[T]
    var size: Int
    var capacity: Int
    var left: Int
    var right: Int

    fn __init__(inout self):
        self.data = AnyPointer[T]()
        self.size = 0
        self.capacity = 0
        self.left = 0
        self.right = 0

    fn __init__(inout self, *, capacity: Int):
        self.data = AnyPointer[T].alloc(capacity)
        self.size = 0
        self.capacity = capacity
        self.left = 0
        self.right = 0

    fn __moveinit__(inout self, owned existing: Self):
        self = Self(capacity=existing.capacity)
        for i in range(len(existing)):
            self.push_back(existing.pop_front())

    fn __del__(owned self):
        for i in range(self.size):
            _ = (self.data + self.left + i).take_value()
        if self.data:
            self.data.free()

    fn __len__(self) -> Int:
        return self.size

    fn _realloc(inout self, new_capacity: Int):
        var new_data = AnyPointer[T].alloc(new_capacity)
        for i in range(self.size):
            (new_data + i).emplace_value((self.data + self.left + i).take_value())

        self.left = 0
        self.right = self.size

        if self.data:
            self.data.free()
        self.data = new_data
        self.capacity = new_capacity

    fn push_back(inout self, owned value: T):
        if self.right >= self.capacity:
            self._realloc(_max(1, self.capacity * 2))
        (self.data + self.right).emplace_value(value ^)
        self.size += 1
        self.right += 1

    fn pop_front(inout self) -> T:
        var ret_val = (self.data + self.left).take_value()
        self.size -= 1
        self.left += 1
        if self.size * 4 < self.capacity:
            if self.capacity > 1:
                self._realloc(self.capacity // 2)
        return ret_val ^
