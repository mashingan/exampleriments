import 'package:flutter/material.dart';

void main() {
  runApp(const MainApp());
}

class MainApp extends StatelessWidget {
  const MainApp({super.key});

  @override
  Widget build(BuildContext context) {
    final colors = [Colors.red, Colors.green, Colors.blue];
    return MaterialApp(
      title: "Flutter Demo",
      theme: ThemeData(
        primarySwatch: Colors.blue,
        useMaterial3: true,
      ),
      home: Scaffold(
        body: Center(
          child: Column(
            mainAxisAlignment: MainAxisAlignment.spaceEvenly,
            children: [
              for (var color in colors)
                RawMaterialButton(
                    constraints: const BoxConstraints(minWidth: 188, minHeight: 136),
                    shape: const CircleBorder(),
                    elevation: 2.0,
                    fillColor: color,
                    padding: const EdgeInsets.all(15),
                    onPressed: () {})
            ],
          ),
        ),
      ),
    );
  }
}
