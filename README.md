# Harmony
_A Cats and Scalaz compatibility library._

Tired of writing separate code for Cats and Scalaz? All that preprocessor noise and those extra packages are too much work! Just pick between Scalaz and Cats. If your clients don't like your choice, they can use one import from this library to become happy.

We can all live together!

## Usage

### SBT dependency

#### cats-core

| cats version | scalaz version | harmony version | dependency |
| ------------ | -------------- | --------------- | ---------- |
| 0.8.1        | 7.2.*          | 1.1          | `"me.jeffshaw.harmony" %% "harmony_cats0-8-1_scalaz7-2" % "1.1"` |
| 0.9.0        | 7.2.*          | 1.1          | `"me.jeffshaw.harmony" %% "harmony_cats0-9-0_scalaz7-2" % "1.1"` |
| 1.0.0-MF    | 7.2.*           | 2.0          | `"me.jeffshaw.harmony" %% "harmony_cats1-0-0-mf_scalaz7-2" % "2.0"` |

#### cats-mtl-core

| cats version | scalaz version | harmony version | dependency |
| ------------ | -------------- | --------------- | ---------- |
| 0.0.2        | 7.2.*          | 2.0          | `"me.jeffshaw.harmony" %% "harmony_cats-mtl-core0-0-2_scalaz7-2" % "2.0"` |

The naming scheme will change if Cats or Scalaz change their binary compatibility guarantees. Cats has no such guarantee.

Harmony releases sharing an artifact name and major version should be binary compatible.

### Imports

| domain | codomain | type class | data types | both |
| ------ | -------- | --------- | ---------- | ---- |
| Cats   | Scalaz   | `import harmony.toscalaz.TypeClasses._` | `import harmony.toscalaz.Data._` | `import harmony.ToScalaz._` |
| Scalaz   | Cats   | `import harmony.tocats.TypeClasses._` | `import harmony.tocats.Data._` | `import harmony.ToCats._` |

Or, if you just want everyone to get along,

`import harmony.Everyone._`

### Examples

Want a `cats.Functor` for `scalaz.IList`?

```scala
import harmony.tocats.typeclass.FunctorConverter._

cats.Functor[scalaz.IList]
```

Want to use a `scalaz.Writer` in a `cats.data.Writer`?

```scala
import cats.instances.int._
import harmony.tocats.data.WriterTConverter._

val tell3 = scalaz.WriterT.tell(3)

val w: cats.data.WriterT[cats.Id, Int, Unit] =
  for {
    _ <- cats.data.Writer.tell(2)
    _ <- tell3
  } yield ()

w.run // yields (5, ())
```

## Provided conversions

Please see the [Google doc](https://docs.google.com/spreadsheets/d/1GCiEnpMJ88Nck7Bw24ef98KMs33RnWQBH29p-b8N60w).

## Something missing?

Please fork this repo, add the relevant conversions to the relevant files, and do a pull request.

Or you can open an issue describing the conversions you need and someone might do it for you.

## Tests

Harmony could use many more tests. As the library gains popularity I will add more.

## Changes

### 2.0

* minimize use of `val` for overrides.
* Conversions that require a second instance require the instance be in the target library, not the source. For example, the converter to `cats.arrow.Choice` requires a `cats.functor.Profunctor`, not a `scalaz.Profunctor`.
* Cats moved some type classes to a separate package, cats-mtl-core. The converters for those type classes are in a new Harmony library.

### 1.1

* Initialization fixes

### 1.0

* Initial release
