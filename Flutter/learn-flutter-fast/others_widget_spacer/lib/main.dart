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
      theme: ThemeData(useMaterial3: true, primarySwatch: Colors.blue),
      home: Scaffold(
        appBar: AppBar(
          actions: [
            const Spacer(),
            const Center(
              child: Text(
                "Spacer",
                style: TextStyle(fontSize: 20),
              ),
            ),
            const Spacer(flex: 5),
            IconButton(
                onPressed: () {}, icon: const Icon(Icons.settings_overscan)),
            const Spacer(),
            IconButton(
                onPressed: () {}, icon: const Icon(Icons.settings_overscan)),
          ],
        ),
        body: const Center(
          child: Column(
            mainAxisAlignment: MainAxisAlignment.center,
            children: [Text("Dummy")],
          ),
        ),
      ),
    );
  }
}
