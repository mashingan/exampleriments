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
      theme: ThemeData(primarySwatch: Colors.blue),
      home: Scaffold(
        appBar: AppBar(title: const Text("Column expanded")),
        body: Center(
          child: Column(
            mainAxisAlignment: MainAxisAlignment.start,
            children: [
              for (var color in colors)
                Expanded(
                    child: RawMaterialButton(
                        elevation: 2, fillColor: color, onPressed: () {}))
            ],
          ),
        ),
      ),
    );
  }
}
