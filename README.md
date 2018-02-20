# struct-indenter

## Example Usage

```bash
stack build
echo 'MyStruct(1,2,3,MyItem("hello, world"), Struct(Array(Struct(Struct()))))' | stack exec struct-indenter-exe
```

Then you will have the following output.

```txt
MyStruct(
  1,
  2,
  3,
  MyItem(
    hello, world
  ),
   Struct(
    Array(
      Struct(
        Struct(

        )
      )
    )
  )
)
```