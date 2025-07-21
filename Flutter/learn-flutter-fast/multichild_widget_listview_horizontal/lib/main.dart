import 'package:flutter/material.dart';

void main() {
  runApp(const MainApp());
}

class MainApp extends StatelessWidget {
  const MainApp({super.key});

  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      title: "Flutter Demo",
      home: Scaffold(
        appBar: AppBar(
          title: const Text("ListView Horizontal"),
        ),
        body: Container(
          margin: const EdgeInsets.symmetric(vertical: 20),
          child: ListView(scrollDirection: Axis.horizontal, children: [
            for (var color in [
              Colors.red,
              Colors.green,
              Colors.blue,
              Colors.yellow,
              Colors.orange
            ])
              Container(width: 160, color: color)
          ]),
        ),
      ),
    );
  }
}
