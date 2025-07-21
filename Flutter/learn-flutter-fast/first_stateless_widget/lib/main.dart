import 'package:flutter/material.dart';
import 'package:get/get.dart';
// import 'dart:ui_web' as ui;
// import 'dart:html';

/*
due to new renderer of web, need to run like this

```
flutter run -d chrome --web-renderer html
flutter build --web-renderer html --release
```

: solution #3

ref: https://stackoverflow.com/a/67270211
*/

void main() {
  runApp(GetMaterialApp(
      home: const MyApp(),
      title: "Flutter Demo",
      theme: ThemeData(
        colorScheme: ColorScheme.fromSeed(seedColor: Colors.deepPurple),
        useMaterial3: true,
      )));
}

const _bmwImageSrc =
    "https://media.ed.edmunds-media.com/bmw/m3/2018/oem/2018_bmw_m3_sedan_base_fq_oem_4_150.jpg";
const _nissanGtrSrc =
    "https://media.ed.edmunds-media.com/nissan/gt-r/2018/oem/2018_nissan_gt-r_coupe_nismo_fq_oem_1_150.jpg";
const _nissanSentraSrc =
    "https://media.ed.edmunds-media.com/nissan/sentra/2017/oem/2017_nissan_sentra_sedan_sr-turbo_fq_oem_4_150.jpg";

class MyApp extends StatelessWidget {
  const MyApp({super.key});

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(title: const Text("Cars")),
      body: ListView(children: [
        CarWidget("BMW", "M3", _bmwImageSrc),
        CarWidget("Nissan", "GTR", _nissanGtrSrc),
        CarWidget("Nissan", "Sentra", _nissanSentraSrc),
      ]),
    );
  }
}

class CarWidget extends StatelessWidget {
  CarWidget(this.make, this.model, this.imageSrc) : super();

  final String make;
  final String model;
  final String imageSrc;

  @override
  Widget build(BuildContext context) {
    return Padding(
      padding: const EdgeInsets.all(10),
      child: Container(
        decoration: BoxDecoration(border: Border.all()),
        padding: const EdgeInsets.all(20),
        child: Center(
          child: Column(
              children: [
                Text("$make $model", style: const TextStyle(fontSize: 24),),
                Padding(
                  padding: const EdgeInsets.only(top: 20),
                  child: Image.network(imageSrc),
                )]),
        ),
      ),
    );
  }
}

// class MyImage extends StatelessWidget {
//   MyImage(this.src) : super();
//   final String src;

//   @override
//   Widget build(BuildContext context) {
//     ui.platformViewRegistry
//         .registerViewFactory(src, (int _) =>
//         ImageElement()
//         ..src = src
//         ..style.width = '10%'
//         ..style.height = "10%");
//     return HtmlElementView(viewType: src);
//   }
// }
