## About SpinalHDL

SpinalHDL is:

 - A language to describe digital hardware
 - Compatible with EDA tools, as it generates VHDL/Verilog files
 - Much more powerful than VHDL, Verilog, and SystemVerilog in its syntax and features
 - Much less verbose than VHDL, Verilog, and SystemVerilog
 - Not an HLS, nor based on the event-driven paradigm
 - Only generates what you asked it in a one-to-one way (no black-magic, no black box)
 - Not introducing area/performance overheads in your design (versus a hand-written VHDL/Verilog design)
 - Based on the RTL description paradigm, but can go much further
 - Allowing you to use Object-Oriented Programming and Functional Programming to elaborate your hardware and verify it
 - Free and can be used in the industry without any license

## Links

 - Documentation                  <br> https://spinalhdl.github.io/SpinalDoc-RTD/
 - Chinese documentation          <br> https://spinalhdl.github.io/SpinalDoc-RTD/zh_CN/
 - Chinese documentation (v1.7.2 from thuCGRA) <br> https://thucgra.github.io/SpinalHDL_Chinese_Doc/
 - API reference                  <br> https://spinalhdl.github.io/SpinalHDL/dev/spinal/index.html
 - Presentation of the language   <br> https://spinalhdl.github.io/SpinalDoc-RTD/master/SpinalHDL/Getting%20Started/presentation.html
 - base projects
     - SBT                            https://github.com/SpinalHDL/SpinalTemplateSbt
     - Gradle                         https://github.com/SpinalHDL/SpinalTemplateGradle
     - Mill                           https://github.com/SpinalHDL/SpinalTemplateMill
 - Jupyter bootcamp               <br> https://github.com/SpinalHDL/Spinal-bootcamp
 - Workshop                       <br> https://github.com/SpinalHDL/SpinalWorkshop
 - Google group                   <br> https://groups.google.com/forum/#!forum/spinalhdl-hardware-description-language
 - Donation channel               <br> https://opencollective.com/spinalhdl

English: [![Join the chat at https://gitter.im/SpinalHDL/SpinalHDL](https://badges.gitter.im/SpinalHDL/SpinalHDL.svg)](https://gitter.im/SpinalHDL/SpinalHDL?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge) 中文: [![Join the chat at https://gitter.im/SpinalHDL-CN/community](https://badges.gitter.im/SpinalHDL/SpinalHDL.svg)](https://gitter.im/SpinalHDL-CN/community?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

## Get it

SpinalHDL is simply a set of Scala libraries. Include them into your project and you're good to go! If you're unsure about what to do, simply clone one of our example projects (see links above).

### SBT (Scala build tool)

```scala
scalaVersion := "2.11.12"

libraryDependencies ++= Seq(
  "com.github.spinalhdl" % "spinalhdl-core_2.11" % "latest.release",
  "com.github.spinalhdl" % "spinalhdl-lib_2.11" % "latest.release",
  compilerPlugin("com.github.spinalhdl" % "spinalhdl-idsl-plugin_2.11" % "latest.release")
)
```

You can force SBT to pick a specific SpinalHDL version by replacing `latest.release` with a specific version.
See the [SpinalHDL SBT Template project's `build.sbt` file](https://github.com/SpinalHDL/SpinalTemplateSbt/blob/master/build.sbt) for a full SBT example.

### Gradle

```kotlin
repositories {
	mavenCentral()
}

dependencies {
	compile group: 'com.github.spinalhdl', name: 'spinalhdl-core_2.11', version: '1.6.4'
	compile group: 'com.github.spinalhdl', name: 'spinalhdl-lib_2.11', version: '1.6.4'
}
```

### Mill (Build Tool)

```scala 
import mill._
import mill.scalalib._

object MySpinalModule extends ScalaModule {
  def scalaVersion = "2.11.12"

  def ivyDeps = Agg(
    ivy"com.github.spinalhdl::spinalhdl-core:1.6.4",
    ivy"com.github.spinalhdl::spinalhdl-lib:1.6.4",
  )

  def scalacPluginIvyDeps = Agg(ivy"com.github.spinalhdl::spinalhdl-idsl-plugin:1.6.4")
}
```

### JAR

    https://oss.sonatype.org/content/groups/public/com/github/spinalhdl/spinalhdl-core_2.11/
    https://oss.sonatype.org/content/groups/public/com/github/spinalhdl/spinalhdl-lib_2.11/

The files are available [on Maven](https://mvnrepository.com/artifact/com.github.spinalhdl) as well.

## Change logs

https://github.com/SpinalHDL/SpinalHDL/tags

## License

The SpinalHDL core is using the LGPL3 license while SpinalHDL lib and others are using the MIT license. That's for the formalities. But there are some practical statements implied by those licenses:

Your freedoms are:

 - You can use SpinalHDL core and lib in your closed/commercial projects.
 - The generated RTL is yours (.vhd/.v files)
 - Your hardware description is yours (.scala files)

Your obligations (and my wish) are:

 - If you modify the SpinalHDL core (the compiler itself), please, share your improvements.

Also, SpinalHDL is provided "as is", without warranty of any kind.
