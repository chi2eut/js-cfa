function f(x) {
    function g(y) {
        y + z
    }

    g(x)
    let z = 2
}

console.log(f(42))

