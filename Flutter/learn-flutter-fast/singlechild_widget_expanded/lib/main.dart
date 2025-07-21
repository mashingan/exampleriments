import 'package:flutter/material.dart';
import 'package:get/get.dart';

void main() {
  runApp(GetMaterialApp(
    title: "Expanded Widget",
    home: const MainApp(),
    theme: ThemeData(useMaterial3: true, primarySwatch: Colors.blue),
  ));
}

class AppState extends GetxController {
  var topExpanded = false.obs;
  var bottomExpanded = false.obs;

  toggleTop() => topExpanded.toggle();
  toggleBottom() => bottomExpanded.toggle();
}

class MainApp extends StatelessWidget {
  const MainApp({super.key});

  @override
  Widget build(BuildContext context) {
    final AppState aps = Get.put(AppState());
    var topContainer = Container(
      decoration: BoxDecoration(
        border: Border.all(color: Colors.black, width: 1),
        color: Colors.blue,
      ),
      padding: const EdgeInsets.all(10),
      child: const Text("Top Container"),
    );
    var bottomContainer = Container(
      decoration: BoxDecoration(
        border: Border.all(color: Colors.black, width: 1),
        color: Colors.yellow,
      ),
      padding: const EdgeInsets.all(10),
      child: const Text("Bottom Container"),
    );
    var topWidget = Obx(() =>
        aps.topExpanded.isTrue ? Expanded(child: topContainer) : topContainer);
    var bottomWidget = Obx(() => aps.bottomExpanded.isTrue
        ? Expanded(child: bottomContainer)
        : bottomContainer);
    return Scaffold(
      appBar: AppBar(
        title: const Text("Expanded"),
        actions: [
          TextButton.icon(
              onPressed: aps.toggleTop,
              icon: Obx(() => Icon(aps.topExpanded.isTrue
                  ? Icons.expand_more
                  : Icons.expand_less)),
              label: const Text("Top")),
          TextButton.icon(
              onPressed: aps.toggleBottom,
              icon: Obx(() => Icon(aps.bottomExpanded.isTrue
                  ? Icons.expand_more
                  : Icons.expand_less)),
              label: const Text("Bottom")),
        ],
      ),
      body: Center(
        child: Column(
          mainAxisAlignment: MainAxisAlignment.center,
          children: [topWidget, bottomWidget],
        ),
      ),
    );
  }
}
