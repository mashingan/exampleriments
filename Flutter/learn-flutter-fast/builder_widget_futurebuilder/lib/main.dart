import 'package:flutter/material.dart';
import 'package:get/get.dart';

void main() {
  runApp(GetMaterialApp(
    title: "Flutter Demo",
    theme: ThemeData(primarySwatch: Colors.blue, useMaterial3: true),
    home: const MainApp(),
  ));
}

class MainAppState extends GetxController {
  var count = 0.obs;
  var showCalculation = false.obs;
}

class MainApp extends StatelessWidget {
  const MainApp({super.key});

  String computeListOfTimestamps(int count) {
    var result = StringBuffer();
    for (int i = 0; i < count; i++) {
      result.writeln("${i + 1}: ${DateTime.now()}");
    }
    return result.toString();
  }

  Future<String> createFutureCalculation(int count) {
    return Future(() => computeListOfTimestamps(count));
  }

  @override
  Widget build(BuildContext context) {
    final MainAppState mas = Get.put(MainAppState());

    var child = Obx(() {
      return mas.showCalculation.isTrue
          ? FutureBuilder(
              future: createFutureCalculation(10000),
              builder: ((context, snapshot) => Expanded(
                      child: SingleChildScrollView(
                    child: Text(
                      "${snapshot.data}",
                      style: const TextStyle(fontSize: 20),
                    ),
                  ))))
          : const Text("Hit the button to show calculation");
    });

    return Scaffold(
      appBar: AppBar(title: const Text("FutureBuilder")),
      body: Center(
        child: Column(
          mainAxisAlignment: MainAxisAlignment.center,
          children: [child],
        ),
      ),
      floatingActionButton: FloatingActionButton(
        onPressed: () => mas.showCalculation.toggle(),
        tooltip: "Invoke Future",
        child: const Icon(Icons.refresh),
      ),
    );
  }
}
