import 'dart:math';

import 'package:flutter/material.dart';
import 'package:get/get.dart';

void main() {
  runApp(GetMaterialApp(
    title: "Positioned Stack",
    theme: ThemeData(primarySwatch: Colors.blue, useMaterial3: true),
    home: const MainApp(),
  ));
}

class MainAppState extends GetxController {
  var top = 0.0.obs;
  var left = 0.0.obs;
  var wlist = <Widget>[].obs;

  final rand = Random();

  int next(int min, int max) => min + rand.nextInt(max - min);
  addLayer() {
    wlist.add(Positioned(
        left: left.value,
        top: top.value,
        child: Container(
          width: 100,
          height: 100,
          decoration: BoxDecoration(
              border: Border.all(color: Colors.grey, width: 2),
              color: Color.fromRGBO(
                  next(0, 255), next(0, 255), next(0, 255), 0.5)),
        )));
    top.value += 30;
    left.value += 30;
  }
}

class MainApp extends StatelessWidget {
  const MainApp({super.key});

  @override
  Widget build(BuildContext context) {
    final MainAppState mas = Get.put(MainAppState());
    return Scaffold(
      appBar: AppBar(title: const Text("Positioned")),
      body: Obx(() => Stack(children: mas.wlist.toList())),
      floatingActionButton: FloatingActionButton(
        onPressed: mas.addLayer,
        tooltip: "Increment",
        child: const Icon(Icons.add),
      ),
    );
  }
}
