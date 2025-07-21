import 'dart:math';

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
      theme: ThemeData(primarySwatch: Colors.blue, useMaterial3: true),
      home: Scaffold(
        body: CircleApp(),
      ),
    );
  }
}

class CirclePainter extends CustomPainter {
  final _random = Random();
  final _colors = <Color>[];
  CirclePainter() {
    for (int i = 0; i < 100; i++) {
      _colors.add(Colors.green
          .withRed(next(0, 255))
          .withGreen(next(0, 255))
          .withBlue(next(0, 255)));
    }
  }
  int next(int min, int max) => min + _random.nextInt(max - min);

  @override
  bool shouldRepaint(CirclePainter oldDelegate) {
    return false;
  }

  @override
  void paint(Canvas canvas, Size size) {
    for (int i = 0; i < 100; i++) {
      var radius = (i * 10).toDouble();
      canvas.drawCircle(
          const Offset(1000, 1000),
          radius,
          Paint()
            ..color = _colors[i]
            ..strokeCap = StrokeCap.round
            ..style = PaintingStyle.stroke
            ..strokeWidth = 15);
    }
  }
}

class CircleApp extends StatelessWidget {
  CircleApp({super.key});
  final cp = CirclePainter();
  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(title: const Text("Scroll Horizontal")),
      body: SingleChildScrollView(
          scrollDirection: Axis.horizontal,
          physics: const AlwaysScrollableScrollPhysics(),
          child: CustomPaint(
            size: const Size(2000, 20000),
            foregroundPainter: cp,
          )),
    );
  }
}
