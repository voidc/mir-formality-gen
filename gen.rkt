(unknown-item #0)

(unknown-item std#2)

(static UNIT () where () : (user-ty (& static (& static ()))) = (∃ () {
  ((_0 (user-ty (& ? (& ? ()))) mut)
   (_1 (user-ty (& ? (& ? ()))) ())
   (_2 (user-ty (& ? ())) ())
   (_3 (user-ty ()) ()))

  ((bb0 {
     ((storage-live _1)
      (storage-live _2)
      (storage-live _3)
      (_3 = (unknown-rvalue ()))
      (_2 = (ref ? () _3))
      (_1 = (ref ? () _2))
      (_0 = (ref ? () (* _1)))
      (storage-dead _1))
     return
   }))
}))

(fn foo ((type T) (lifetime 'a) (lifetime 'b)) ((user-ty (& 'a (& 'b ()))) (user-ty (& 'b T))) -> (user-ty (& 'a T))
  where (((user-ty T) : std::marker::Sized ()))
  (∃ () {
  ((_0 (user-ty (& ? T)) mut)
   (_1 (user-ty (& ? (& ? ()))) mut)
   (_2 (user-ty (& ? T)) ()))

  ((bb0 {
     ((_0 = (ref ? () (* _2))))
     return
   }))
}))

(fn bad ((type T) (lifetime 'a)) ((user-ty (& 'a T))) -> (user-ty (& static T))
  where (((user-ty T) : std::marker::Sized ()))
  (∃ () {
  ((_0 (user-ty (& ? T)) mut)
   (_1 (user-ty (& ? T)) ())
   (_2 (user-ty (fn ((& ? (& ? ())) (& ? T)) -> (& ? T))) ())
   (_3 (user-ty (fn ((& ? (& ? ())) (& ? T)) -> (& ? T))) mut)
   (_4 (user-ty (& ? (& ? ()))) mut)
   (_5 (user-ty (& ? (& ? (& ? ())))) ())
   (_6 (user-ty (& ? T)) mut))

  ((bb0 {
     ((storage-live _2)
      (_2 = (unknown-rvalue foo::<T> as for<'a, 'b> fn(&'a &'b (), &'b T) -> &'a T (Pointer(ReifyFnPointer))))
      unknown-stmt
      unknown-stmt
      (storage-live _3)
      (_3 = (use (copy _2)))
      (storage-live _4)
      (storage-live _5)
      (_5 = (use (const pointer to alloc1)))
      (_4 = (ref ? () (* (* _5))))
      (storage-live _6)
      (_6 = (use (copy _1))))
     (call (move _3) (move _4) (move _6) _0 (bb1))
   })
   (bb1 {
     ((storage-dead _6)
      (storage-dead _4)
      (storage-dead _3)
      (storage-dead _2)
      (storage-dead _5))
     return
   })
   (bb2 {
     ()
     resume
   }))
}))

(fn main () () -> (user-ty ())
  where ()
  (∃ () {
  ((_0 (user-ty ()) mut)
   (_1 (user-ty (& ? i32)) ())
   (_2 (user-ty i32) ())
   (_3 (user-ty (& ? i32)) mut)
   (_4 (user-ty (& ? i32)) ()))

  ((bb0 {
     ((storage-live _1)
      (storage-live _2)
      (_2 = (use (const 22)))
      unknown-stmt
      (storage-live _3)
      (storage-live _4)
      (_4 = (ref ? () _2))
      (_3 = (ref ? () (* _4))))
     (call (const 0) (move _3) _1 (bb1))
   })
   (bb1 {
     ((storage-dead _3)
      (storage-dead _2)
      unknown-stmt
      (storage-dead _4)
      (_0 = (use (const 0)))
      (storage-dead _1))
     return
   })
   (bb2 {
     ()
     resume
   }))
}))


