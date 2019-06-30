mod module;
use module::engine::*;

fn main() {
    let mut engine_vm = Engine::new();
    engine_vm.print_engine();
    engine_vm.register_fn("test",test_fn);
    //engine_vm.call_fn("a".to_string(), vec![1,2,3]);
    println!("Hello, world!");
}

fn test_fn (a:i32,b:i32,c:i32){
    println!("test");
}