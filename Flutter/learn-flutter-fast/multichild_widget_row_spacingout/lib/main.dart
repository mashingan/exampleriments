import 'package:flutter/material.dart';

void main() {
  runApp(const MainApp());
}

class MainApp extends StatelessWidget {
  const MainApp({super.key});

  @override
  Widget build(BuildContext context) {
    final alignment = [
      MainAxisAlignment.center,
      MainAxisAlignment.end,
      MainAxisAlignment.spaceAround,
      MainAxisAlignment.spaceEvenly,
      MainAxisAlignment.spaceBetween,
      MainAxisAlignment.start
    ];
    return MaterialApp(
      title: "Flutter Demo",
      theme: ThemeData(
        useMaterial3: true,
        primarySwatch: Colors.blue,
      ),
      home: Scaffold(
        appBar: AppBar(
          title: const Text("Rows"),
        ),
        body:
            Column(mainAxisAlignment: MainAxisAlignment.spaceEvenly, children: [
          for (var align in alignment)
            Row(
              mainAxisAlignment: align,
              children: [
                const Text("MainAxisAlignment"),
                const Text("is"),
                Text(align.toString().split(".")[1]),
              ],
            )
        ]),
      ),
    );
  }
}
