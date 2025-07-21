import 'package:flutter/material.dart';
import 'package:get/get.dart';

void main() {
  runApp(GetMaterialApp(
      title: "Gesture App",
      theme: ThemeData(useMaterial3: true, primarySwatch: Colors.blue),
      home: const MainApp()));
}

class MainAppState extends GetxController {
  var log = "".obs;
  set logGesture(String logtext) {
    log.value += "\n$logtext";
  }

  clear() => log.value = "";
}

class MainApp extends StatelessWidget {
  const MainApp({super.key});

  @override
  Widget build(BuildContext context) {
    final MainAppState mas = Get.put(MainAppState());
    return Scaffold(
      appBar: AppBar(title: const Text("Gesture")),
      body: Center(
        child:
            Column(mainAxisAlignment: MainAxisAlignment.spaceEvenly, children: [
          GestureDetector(
            child: const Text("Gesture me"),
            onTap: () => mas.logGesture = "tap",
            onTapDown: (details) => mas.logGesture = "onTapDown: $details\n",
            onTapUp: (details) => mas.logGesture = "onTapUp: $details\n",
            onTapCancel: () => mas.logGesture = "onTapCancel",
            onDoubleTap: () => mas.logGesture = "onDoubleTap",
            onLongPress: () => mas.logGesture = "onLongPress",
            onVerticalDragStart: (details) =>
                mas.logGesture = "onVerticalDragStart: $details",
            onVerticalDragUpdate: (details) =>
                mas.logGesture = "onVerticalDragUpdate: $details",
            onVerticalDragEnd: (details) =>
                mas.logGesture = "onVerticalDragEnd: $details",
            onVerticalDragCancel: () => mas.logGesture = "onVerticalDragCancel",
            onHorizontalDragStart: (details) =>
                mas.logGesture = "onHorizontalDragStart: $details",
            onHorizontalDragUpdate: (details) =>
                mas.logGesture = "onHorizontalDragUpdate: $details",
            onHorizontalDragEnd: (details) =>
                mas.logGesture = "onHorizontalDragEnd: $details",
            onHorizontalDragCancel: () =>
                mas.logGesture = "onHorizontalDragCancel",
          ),
          Obx(() => Container(
            constraints: const BoxConstraints(maxHeight: 200),
            decoration:
                BoxDecoration(border: Border.all(color: Colors.grey, width: 1)),
            margin: const EdgeInsets.all(10),
            padding: const EdgeInsets.all(10),
            child: SingleChildScrollView(
                child: Text("${mas.log}")),
          )),
          ElevatedButton(onPressed: mas.clear, child: const Text("Clear"))
        ]),
      ),
    );
  }
}
