(rust_out (crate [
(unknown-item #0)

(unknown-item std#2)

(static UNIT[] where () : (user-ty (& static (& static ()))) =
 (∃ [(lifetime ?0)
     (lifetime ?1)
     (lifetime ?2)
     (lifetime ?3)
     (lifetime ?4)
     (lifetime ?5)
     (lifetime ?6)
     (lifetime ?7)
     (lifetime ?8)
     (lifetime ?9)] {
  [(_0 (user-ty (& ?5 (& ?6 ()))) mut)
   (_1 (user-ty (& ?7 (& ?8 ()))) ())
   (_2 (user-ty (& ?9 ())) ())
   (_3 (user-ty ()) ())]

  [(bb0 {
     [(storage-live _1)
      (storage-live _2)
      (storage-live _3)
      (_3 = (unknown-rvalue ()))
      (_2 = (ref ?0 () _3))
      (_1 = (ref ?1 () _2))
      (_0 = (ref ?2 () (* _1)))
      (storage-dead _1)]
     return
   })]
}))

(fn foo[(type T) (lifetime 'a) (lifetime 'b)] ((user-ty (& 'a (& 'b ()))) (user-ty (& 'b T))) -> (user-ty (& 'a T))
 where [((user-ty T) : std::marker::Sized[])]
 (∃ [(lifetime ?0)
     (lifetime ?1)
     (lifetime ?2)
     (lifetime ?3)
     (lifetime ?4)
     (lifetime ?5)] {
  [(_0 (user-ty (& ?2 T)) mut)
   (_1 (user-ty (& ?3 (& ?4 ()))) mut)
   (_2 (user-ty (& ?5 T)) ())]

  [(bb0 {
     [(_0 = (ref ?0 () (* _2)))]
     return
   })]
}))

(fn bad[(type T) (lifetime 'a)] ((user-ty (& 'a T))) -> (user-ty (& static T))
 where [((user-ty T) : std::marker::Sized[])]
 (∃ [(lifetime ?0)
     (lifetime ?1)
     (lifetime ?2)
     (lifetime ?3)
     (lifetime ?4)
     (lifetime ?5)
     (lifetime ?6)
     (lifetime ?7)
     (lifetime ?8)
     (lifetime ?9)
     (lifetime ?10)
     (lifetime ?11)
     (lifetime ?12)
     (lifetime ?13)
     (lifetime ?14)
     (lifetime ?15)
     (lifetime ?16)
     (lifetime ?17)
     (lifetime ?18)
     (lifetime ?19)
     (lifetime ?20)
     (lifetime ?21)
     (lifetime ?22)
     (lifetime ?23)
     (lifetime ?24)
     (lifetime ?25)
     (lifetime ?26)
     (lifetime ?27)
     (lifetime ?28)] {
  [(_0 (user-ty (& ?5 T)) mut)
   (_1 (user-ty (& ?6 T)) ())
   (_2 (user-ty (fn ((& ?7 (& ?8 ())) (& ?9 T)) -> (& ?10 T))) ())
   (_3 (user-ty (fn ((& ?11 (& ?12 ())) (& ?13 T)) -> (& ?14 T))) mut)
   (_4 (user-ty (& ?15 (& ?16 ()))) mut)
   (_5 (user-ty (& ?17 (& ?18 (& ?19 ())))) ())
   (_6 (user-ty (& ?20 T)) mut)]

  [(bb0 {
     [(storage-live _2)
      (_2 = (unknown-rvalue foo::<T> as for<'a, 'b> fn(&'a &'b (), &'b T) -> &'a T (Pointer(ReifyFnPointer))))
      unknown-stmt
      unknown-stmt
      (storage-live _3)
      (_3 = (use (copy _2)))
      (storage-live _4)
      (storage-live _5)
      (_5 = (use (const pointer to alloc1)))
      (_4 = (ref ?3 () (* (* _5))))
      (storage-live _6)
      (_6 = (use (copy _1)))]
     (call (move _3) (move _4) (move _6) _0 (bb1))
   })
   (bb1 {
     [(storage-dead _6)
      (storage-dead _4)
      (storage-dead _3)
      (storage-dead _2)
      (storage-dead _5)]
     return
   })
   (bb2 {
     []
     resume
   })]
}))

(fn main[] () -> (user-ty ())
 where []
 (∃ [(lifetime ?0)
     (lifetime ?1)
     (lifetime ?2)
     (lifetime ?3)
     (lifetime ?4)] {
  [(_0 (user-ty ()) mut)
   (_1 (user-ty (& ?2 i32)) ())
   (_2 (user-ty i32) ())
   (_3 (user-ty (& ?3 i32)) mut)
   (_4 (user-ty (& ?4 i32)) ())]

  [(bb0 {
     [(storage-live _1)
      (storage-live _2)
      (_2 = (use (const 22)))
      unknown-stmt
      (storage-live _3)
      (storage-live _4)
      (_4 = (ref ?0 () _2))
      (_3 = (ref ?1 () (* _4)))]
     (call (const 0) (move _3) _1 (bb1))
   })
   (bb1 {
     [(storage-dead _3)
      (storage-dead _2)
      unknown-stmt
      (storage-dead _4)
      (_0 = (use (const 0)))
      (storage-dead _1)]
     return
   })
   (bb2 {
     []
     resume
   })]
}))]))
