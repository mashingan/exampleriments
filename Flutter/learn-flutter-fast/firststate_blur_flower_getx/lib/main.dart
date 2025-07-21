import 'dart:ui';

import 'package:flutter/material.dart';
import 'package:get/get.dart';

const _imageClear =
    "https://upload.wikimedia.org/wikipedia/commons/9/9f/LiliumAuratumVVirginaleBluete2Rework.jpg";
const _imageBlur =
    "https://images.unsplash.com/photo-1531603071569-0dd65ad72d53?ixlib=rb-1.2.1&ixid=eyJhcHBfaWQiOjEyMDd9&w=1000&q=80";

void main() {
  // runApp(MyApp());
  runApp(GetMaterialApp(
    home: FlowerApp(),
    title: "Flutter Demo",
    theme: ThemeData(
      useMaterial3: true,
      primarySwatch: Colors.blue,
    ),
  ));
}

class AppState extends GetxController {
  var bright = false.obs;
  var blur = 0.0.obs;
  var imgsrc = "".obs;
  blurMore() => blur.value += 5.0;
  set img(String src) => imgsrc.value = src;
  toggle() {
    bright.toggle();
    if (bright.isTrue) {
      imgsrc.value = _imageClear;
    } else {
      imgsrc.value = _imageBlur;
    }
    blur.value = 0;
  }
}

class MyApp extends StatelessWidget {
  @override
  Widget build(BuildContext context) {
    final AppState aps = Get.put(AppState());
    return Obx(() => MaterialApp(
          home: FlowerApp(),
          title: "Flutter Demo",
          theme: ThemeData(
            useMaterial3: true,
            primarySwatch: Colors.blue,
            // brightness: aps.bright ? Brightness.light : Brightness.dark,
          ),
        ));
  }
}

class FlowerApp extends StatelessWidget {
  FlowerApp({Key? key}) : super();

  @override
  Widget build(BuildContext context) {
    final AppState aps = Get.put(AppState());
    // final AppState aps = Get.find();
    aps.toggle();
    return Scaffold(
        appBar: AppBar(
          title: const Text("Flower"),
          actions: [
            IconButton(onPressed: aps.toggle, icon: const Icon(Icons.refresh))
          ],
        ),
        body: Obx(() => Container(
              decoration: BoxDecoration(
                  color: Theme.of(context).colorScheme.background,
                  image: DecorationImage(
                      image: NetworkImage("${aps.imgsrc}"), fit: BoxFit.cover)),
              child: BackdropFilter(
                filter: ImageFilter.blur(
                    sigmaX: aps.blur.value, sigmaY: aps.blur.value),
                child: Container(
                    decoration:
                        BoxDecoration(color: Colors.white.withOpacity(0))),
              ),
            )),
        floatingActionButton: FloatingActionButton(
          onPressed: aps.blurMore,
          tooltip: "Blur more",
          child: const Icon(Icons.add),
        ));
  }
}
