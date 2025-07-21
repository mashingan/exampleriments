import 'package:flutter/material.dart';
import 'package:get/get.dart';

void main() {
  runApp(GetMaterialApp(
    title: "Flutter Demo",
    theme: ThemeData(useMaterial3: true, primarySwatch: Colors.blue),
    home: const MainApp(),
  ));
}

class StackState extends GetxController {
  var waiting = false.obs;
  var children = <Widget>[].obs;
  final _pleaseWaitWidget = const PleaseWaitWidget(
    key: ObjectKey("pleaseWaitWidget"),
  );
  final _appWidget = const AppWidget(
    key: ObjectKey("appWidget"),
  );

  @override
  void onInit() {
    super.onInit();
    children.add(_appWidget);
  }
}

class MainApp extends StatelessWidget {
  const MainApp({super.key});

  @override
  Widget build(BuildContext context) {
    final StackState ss = Get.put(StackState());
    return Scaffold(
      appBar: AppBar(title: const Text("Card Layout Demo")),
      body: Center(
          child: Obx(
        () => Stack(
          key: const ObjectKey("stack"),
          children: ss.children.toList(),
        ),
      )),
      floatingActionButton: FloatingActionButton.extended(
          label: const Text("Please Wait On/Off"),
          icon: const Icon(Icons.cached),
          onPressed: () {
            ss.waiting.toggle();
            ss.children.clear();
            if (ss.waiting.isTrue) {
              ss.children.add(ss._pleaseWaitWidget);
            }
            ss.children.add(ss._appWidget);
          }),
    );
  }
}

class PleaseWaitWidget extends StatelessWidget {
  const PleaseWaitWidget({Key? key}) : super(key: key);
  @override
  Widget build(BuildContext context) {
    return Container(
      color: Colors.grey.withOpacity(0.3),
      child: const Center(
        child: CircularProgressIndicator(strokeWidth: 8),
      ),
    );
  }
}

class AppWidget extends StatelessWidget {
  const AppWidget({Key? key}) : super(key: key);

  @override
  Widget build(BuildContext context) {
    return Center(
      child:
          Column(mainAxisAlignment: MainAxisAlignment.spaceEvenly, children: [
        for (var t in ["your", "App", "Goes", "Here"])
          Text(
            t,
            style: const TextStyle(fontSize: 20),
          )
      ]),
    );
  }
}
