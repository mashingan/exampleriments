import 'package:flutter/material.dart';

void main() {
  runApp(const IconApp());
}

class IconApp extends StatelessWidget {
  const IconApp({super.key});

  @override
  Widget build(BuildContext context) {
    var row1 = const Row(
      mainAxisAlignment: MainAxisAlignment.center,
      children: [Icon(Icons.add), Text("Default size 24, default color black")],
    );
    var row2 = const Row(
      mainAxisAlignment: MainAxisAlignment.center,
      children: [
        Icon(
          Icons.add,
          size: 48,
        ),
        Text("Specified size 48, default color black")
      ],
    );
    var row3 = const Row(
      mainAxisAlignment: MainAxisAlignment.center,
      children: [
        Icon(
          Icons.add,
          size: 96,
          color: Colors.red,
        ),
        Text("Specified size 96, specified color red")
      ],
    );
    return MaterialApp(
      home: Scaffold(
        appBar: AppBar(
          title: const Text("Icons"),
        ),
        body: Column(
          mainAxisAlignment: MainAxisAlignment.center,
          children: [row1, row2, row3],
        ),
      ),
    );
  }
}
