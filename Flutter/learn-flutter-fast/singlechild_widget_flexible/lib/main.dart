import 'package:flutter/material.dart';
import 'package:get/get.dart';

void main() {
  runApp(GetMaterialApp(
    title: "Flexible Widget",
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
      constraints: const BoxConstraints(
          minHeight: 100, minWidth: 100, maxHeight: 200, maxWidth: 200),
      decoration: BoxDecoration(
        border: Border.all(color: Colors.black, width: 1),
        color: Colors.blue,
      ),
      padding: const EdgeInsets.all(10),
      child: const Text("Top Container"),
    );
    var bottomContainer = Container(
      constraints: const BoxConstraints(
          minHeight: 100, minWidth: 100, maxHeight: 200, maxWidth: 200),
      decoration: BoxDecoration(
        border: Border.all(color: Colors.black, width: 1),
        color: Colors.yellow,
      ),
      padding: const EdgeInsets.all(10),
      child: const Text("Bottom Container"),
    );
    var topWidget = Obx(() => Flexible(
          fit: aps.topExpanded.isTrue ? FlexFit.tight : FlexFit.loose,
          child: topContainer,
        ));
    var bottomWidget = Obx(() => aps.bottomExpanded.isTrue
        ? Expanded(child: bottomContainer)
        : bottomContainer);
    var toolbarTextTop =
        Obx(() => Text("Top ${aps.topExpanded.isTrue ? 'tight' : 'loose'}"));
    var toolbarTextBottom = Obx(() =>
        Text("Bottom ${aps.bottomExpanded.isFalse ? 'not' : ''} expanded"));
    return Scaffold(
      appBar: AppBar(
        title: const Text("Flexible"),
        actions: [
          TextButton.icon(
              onPressed: aps.toggleTop,
              icon: Obx(() => Icon(aps.topExpanded.isTrue
                  ? Icons.keyboard_arrow_up
                  : Icons.keyboard_arrow_down)),
              label: toolbarTextTop),
          TextButton.icon(
              onPressed: aps.toggleBottom,
              icon: Obx(() => Icon(aps.bottomExpanded.isTrue
                  ? Icons.keyboard_arrow_up
                  : Icons.keyboard_arrow_down)),
              label: toolbarTextBottom),
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
