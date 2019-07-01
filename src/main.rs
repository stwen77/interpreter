mod module;
use module::engine::*;

fn main() {
    let mut engine_vm = Engine::new();
    engine_vm.print_engine();
    engine_vm.register_fn("test", test_fn);
    let (mut a, mut b, mut c) = (1, 2, 3);
    engine_vm.call_fn("test".to_string(), vec![&mut a, &mut b, &mut c]);
    println!("Hello, world!");
}

fn test_fn(a: i32, b: i32, c: i32) {
    println!("test");
}
