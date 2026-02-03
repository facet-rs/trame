//  {
//     use super::LArena;

//     #[test]
//     fn live_alloc_and_get() {
//         let mut arena = LArena::new();
//         let id = arena.alloc(42u32);

//         assert!(id.is_valid());
//         assert_eq!(*arena.get(id), 42);
//     }

//     #[test]
//     fn live_free_and_reuse() {
//         let mut arena = LArena::new();

//         let id1 = arena.alloc(1u32);
//         let _id2 = arena.alloc(2u32);

//         let val = arena.free(id1);
//         assert_eq!(val, 1);

//         // Next alloc reuses freed slot
//         let id3 = arena.alloc(3u32);
//         assert_eq!(id3.raw, id1.raw);
//         assert_eq!(*arena.get(id3), 3);
//     }

//     #[test]
//     #[should_panic(expected = "double-free")]
//     fn live_double_free_panics() {
//         let mut arena = LArena::new();
//         let id = arena.alloc(1u32);
//         arena.free(id);
//         arena.free(id);
//     }

//     #[test]
//     fn live_get_mut() {
//         let mut arena = LArena::new();
//         let id = arena.alloc(1u32);

//         *arena.get_mut(id) = 99;
//         assert_eq!(*arena.get(id), 99);
//     }
// }
