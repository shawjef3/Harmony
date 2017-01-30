# Harmony
_A Cats and Scalaz compatibility library._

Tired of writing separate code for Cats and Scalaz? All that preprocessor noise and those extra packages are too much work! Just pick between Scalaz and Cats. If your clients don't like your choice, they can use one import from this library to become happy.

We can all live together!

## Usage

### SBT dependency

`"me.jeffshaw" %% "harmony_cats${catsVersion}_scalaz${scalazMajor}.${scalazMinor}" % "1.0-M0" % "1.0-M0"`

So, for example,

`"me.jeffshaw" %% "harmony_cats0.8.1_scalaz7.2" % "1.0-M0"`

| cats version | scalaz version | harmony version | dependency |
| ------------ | -------------- | --------------- | ---------- |
| 0.8.1        | 7.2.*          | 1.0-M0          | `"me.jeffshaw" %% "harmony_cats0.8.1_scalaz7.2" % "1.0-M0"` |

The naming scheme will change if Cats or Scalaz change their binary compatibility guarantees. Cats has no such guarantee.

Harmony releases sharing an artifact name and major version should be binary compatible.

### Usage

| domain | codomain | data type | type class | both |
| ------ | -------- | --------- | ---------- | ---- |
| Cats   | Scalaz   | `import harmony.toscalaz.DataTypes._` | `import harmony.toscalaz.TypeClasses._` | `import harmony.ToScalaz._` |
| Scalaz   | Cats   | `import harmony.tocats.DataTypes._` | `import harmony.tocats.TypeClasses._` | `import harmony.ToCats._` |

Or, if you just want everyone to get along,

`import harmony.Everyone._`

Want a `cats.Functor` for `scalaz.IList`?

```scala
import harmony.tocats.typeclass.FunctorConverter._

implicitly[cats.Functor[scalaz.IList]]
```

## Provided conversions

Please see the [Google doc](https://docs.google.com/spreadsheets/d/1GCiEnpMJ88Nck7Bw24ef98KMs33RnWQBH29p-b8N60w/edit#gid=524513852).

## Performance Implications

If you are using the same data type for both the source and target, then the performance implications will be essentially zero.

```scala
import harmony.Everyone._
val x = scalaz.OneAnd[Vector, Int](0, Vector(1, 2, 3))

cats.Functor[scalaz.OneAnd[Vector, ?]].map(x)(_.toString)
```

## Something missing?

Please fork this repo, add the relevant conversions to the relevant files, and do a pull request.

Or you can open an issue describing the conversions you need and someone might do it for you.
