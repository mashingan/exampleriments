import 'package:flutter/material.dart';
import 'package:get/get.dart';

void main() {
  runApp(GetMaterialApp(
      title: "Flutter Demo",
      theme: ThemeData(
        primarySwatch: Colors.blue,
      ),
      home: const MainApp(title: "Button Enablement")));
}

class MainAppState extends GetxController {
  var checked = false.obs;
}

class MainApp extends StatelessWidget {
  final String title;
  const MainApp({super.key, required this.title});

  @override
  Widget build(BuildContext context) {
    final MainAppState mas = Get.put(MainAppState());
    return Scaffold(
      appBar: AppBar(
        title: Text(title),
      ),
      body: Center(
        child: Column(
          mainAxisAlignment: MainAxisAlignment.spaceEvenly,
          children: [
            const Text(
              "Please check below to agree to the terms",
              style: TextStyle(fontStyle: FontStyle.italic),
            ),
            Row(
              mainAxisAlignment: MainAxisAlignment.center,
              children: [
                Obx(() => Checkbox(
                    value: mas.checked.value,
                    onChanged: (val) {
                      mas.checked.value = val!;
                    })),
                const Text("I agree"),
              ],
            ),
            Obx(() => OutlinedButton(
                onPressed: mas.checked.value ? () {} : null,
                child: const Text("Register")))
          ],
        ),
      ),
    );
  }
}
