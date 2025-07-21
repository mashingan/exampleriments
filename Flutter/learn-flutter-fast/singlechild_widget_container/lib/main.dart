import 'package:flutter/material.dart';
import 'package:get/get.dart';

void main() {
  runApp(GetMaterialApp(
    title: "Flutter Demo",
    theme: ThemeData(useMaterial3: true, primarySwatch: Colors.blue),
    home: const MainApp(),
  ));
}

class AirplaneState extends GetxController
    with GetSingleTickerProviderStateMixin {
  late AnimationController _controller;
  late Animation<double> _animation;
  @override
  void onInit() {
    _controller =
        AnimationController(duration: const Duration(seconds: 2), vsync: this);
    _animation = Tween<double>(begin: 0.0, end: 1.0).animate(_controller)
      ..addListener(() => update());
    _controller.forward();
    super.onInit();
  }

  @override
  void onClose() {
    super.onClose();
    _controller.dispose();
  }
}

class MainApp extends GetView<AirplaneState> {
  const MainApp({super.key});

  @override
  Widget build(BuildContext context) {
    final AirplaneState ais = Get.put(AirplaneState());
    return GetBuilder<AirplaneState>(
        builder: (_) => Scaffold(
              appBar: AppBar(title: const Text("Airplane rotate")),
              body: Center(
                child: Container(
                  decoration: BoxDecoration(
                      border: Border.all(width: 2, color: Colors.black),
                      borderRadius: const BorderRadius.all(Radius.circular(8)),
                      color: Colors.redAccent),
                  child: RotationTransition(
                    turns: AlwaysStoppedAnimation(ais._animation.value),
                    child: const Icon(
                      Icons.airplanemode_active,
                      size: 150,
                    ),
                  ),
                ),
              ),
              floatingActionButton: FloatingActionButton(
                tooltip: "Increment",
                onPressed: () => ais._controller.forward(from: 0),
                child: const Icon(Icons.rotate_right),
              ),
            ));
  }
}
