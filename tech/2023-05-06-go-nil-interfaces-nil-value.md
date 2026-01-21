---
title: "Go: nil interfaces vs interfaces with nil value"
short-title: "Go and nil interfaces"
date: 2023-05-06
tags:
  - go
  - tech
  - 🇬🇧
---

This is a known gotcha, so let this serve as a reminder, refresher or a warning for newcomers to Go.

Suppose you have a type with a field implementing an interface:

```go
type example struct {
        thing iThing
}

type iThing interface {
        doThing()
}
```

If you set the `thing` field with a `nil`, for example with a constructor that returns a concrete type but returns `nil` in some case, you need to pay attention to the type information of this `nil` value. Chances are it won’t be the same because the type has changed, and any comparison will fail!

```go
type thing struct{}

func (t *thing) doThing() {} // Implementing the iThing interface

// Constructor
func newThing() *thing {
        return nil
}

func main() {
    example1 := example{}
    example1.thing = newThing()

    if example1.thing == nil {
        fmt.Println("example1.thing is nil!")
    } else {
        fmt.Println("example1.thing is NOT nil!")
    }
}
```

What do you think will be the output from this?

```console
example1.thing is NOT nil!
```

Although the value is `nil`, the `nil` is of type `*thing`!

> [!tip]
> An interface, thus,  is `nil` only when both its value and its type are `nil`.

This, on the other hand, would evaluate to true in the above check:

```go
func newIThing() iThing {   // Returns interface
        return nil          // no value, no type either
}

func main() {
    example1 := example{}
    example1.thing = newIThing()

    if example1.thing == nil {
        fmt.Println("example1.thing is nil!")
    } else {
        fmt.Println("example1.thing is NOT nil!")
    }
}
```

This outputs

```console
example1.thing is nil!
```

Comparing the structs generated above will reveal the trick, but trying to output the values could be confusing.

```go
func compare(e1, e2 example) {
    fmt.Printf("Example 1: %v \n", e1)
    fmt.Printf("Example 2: %v \n", e2)

    fmt.Println(e1 == e2)
}

func main() {
    example1 := example{}
    example2 := example{}

    example1.thing = newThing()
    example2.thing = newIThing()

    compare(example1, example2)
}
```

This outputs

```console
Example 1: {<nil>}
Example 2: {<nil>}
false
```

To see the actual difference in the output, use the directive for the Go-syntax representation of the value (`%#v`) instead of the default format `%v` or `%+v`:

```go
func compare(e1, e2 example) {
    fmt.Printf("Example 1: %#v \n", e1)
    fmt.Printf("Example 2: %#v \n", e2)

    fmt.Println(e1 == e2)
}

func main() {
    example1 := example{}
    example2 := example{}

    example1.thing = newThing()
    example2.thing = newIThing()

    compare(example1, example2)
}
```

Now you will be able to see it clearly:

```console
Example 1: main.example{thing:(*main.thing)(nil)}
Example 2: main.example{thing:main.iThing(nil)}
false
```

You can test this in the [Go Playground](https://go.dev/play/p/XG00TeGaOlZ)

Be careful out there! If you follow the rule of "take interfaces, return concrete types", as the [ireturn](https://github.com/butuzov/ireturn) linter will enforce, take this into account when testing an interface variable for `nil`.

Additional reading:

- <https://glucn.medium.com/golang-an-interface-holding-a-nil-value-is-not-nil-bb151f472cc7>
- <http://devs.cloudimmunity.com/gotchas-and-common-mistakes-in-go-golang/index.html#nil_in_nil_in_vals>
