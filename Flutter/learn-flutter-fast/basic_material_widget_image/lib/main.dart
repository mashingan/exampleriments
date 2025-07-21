import 'package:flutter/material.dart';

void main() {
  runApp(const LoadingImageApp());
}

class LoadingImageApp extends StatelessWidget {
  const LoadingImageApp({super.key});
  final imgsrc =
      "https://upload.wikimedia.org/wikipedia/commons/thumb/b/bd/Emmanuel_College_Front_Court%2C_Cambridge%2C_UK_-_Diliff.jpg/1024px-Emmanuel_College_Front_Court%2C_Cambridge%2C_UK_-_Diliff.jpg";

  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      home: Scaffold(
        appBar: AppBar(
          title: const Text("Image"),
        ),
        body: Center(
          child: FadeInImage.assetNetwork(
              placeholder: "assets/loading.gif", image: imgsrc),
        ),
      ),
    );
  }
}
