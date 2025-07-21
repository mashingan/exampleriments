import 'package:flutter/material.dart';
import 'package:get/get.dart';

void main() {
  runApp(GetMaterialApp(
    home: const MainApp(),
    title: "Flutter Demo",
    theme: ThemeData(
      primarySwatch: Colors.blue
    ),
  ));
}

class MainAppState extends GetxController {}

class MainApp extends StatelessWidget {
  const MainApp({super.key});

  @override
  Widget build(BuildContext context) {
    return const MaterialApp(
      home: Scaffold(
        body: Center(
          child: Text('Hello World!'),
        ),
      ),
    );
  }
}
