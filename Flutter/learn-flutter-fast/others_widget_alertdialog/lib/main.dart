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
  var counter = 0.obs;
}

class MainApp extends StatelessWidget {
  const MainApp({super.key});

  Future<bool?> _showConfirmDialog(BuildContext context) async {
    return await showDialog<bool>(
        context: context,
        builder: (BuildContext context) {
          return AlertDialog(
            title: const Text("Confirm"),
            content: const Text("Are you sure want to increment" " counter?"),
            actions: [
              TextButton(
                onPressed: () => Get.back(result: true),
                child: const Text("Yes"),
              ),
              TextButton(
                onPressed: () => Get.back(result: false),
                child: const Text("No"),
              ),
            ],
          );
        });
  }

  _incCounter(BuildContext context, MainAppState mas) {
    _showConfirmDialog(context).then((value) {
      if (value!) mas.counter++;
    });
  }

  @override
  Widget build(BuildContext context) {
    final MainAppState mas = Get.put(MainAppState());
    return Scaffold(
      appBar: AppBar(title: const Text("AlertDialog")),
      body: Center(
        child: Center(
          child: Column(
            mainAxisAlignment: MainAxisAlignment.center,
            children: [
              const Text("You have pushed the button this many times:"),
              Obx(
                () => Text(
                  "${mas.counter}",
                  style: Theme.of(context).textTheme.displaySmall,
                ),
              )
            ],
          ),
        ),
      ),
      floatingActionButton: FloatingActionButton(
        onPressed: () => _incCounter(context, mas),
        tooltip: "Increment",
        child: const Icon(Icons.add),
      ),
    );
  }
}
