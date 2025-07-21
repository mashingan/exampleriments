import 'package:flutter/material.dart';
import 'package:flutter/services.dart';
import 'package:get/get.dart';

void main() {
  runApp(GetMaterialApp(
      home: const MainApp(),
      title: "Flutter Demo",
      theme: ThemeData(useMaterial3: true, primarySwatch: Colors.blue)));
}

enum PopupMenuAction { add1, add10, add100, exit }

class MainAppState extends GetxController {
  var counter = 0.obs;

  inc(int by) {
    counter += by;
  }
}

class MainApp extends StatelessWidget {
  void Function(PopupMenuAction) _onPopupMenuSelected(MainAppState mas) {
    return (PopupMenuAction item) {
      if (PopupMenuAction.exit == item) {
        SystemChannels.platform.invokeMethod('SystemNavigator.pop');
      }
      mas.inc(item == PopupMenuAction.add1
          ? 1
          : item == PopupMenuAction.add10
              ? 10
              : 100);
    };
  }

  const MainApp({super.key});

  @override
  Widget build(BuildContext context) {
    final MainAppState mas = Get.put(MainAppState());
    return Scaffold(
      appBar: AppBar(
        title: const Text("PopupMenuButton Demo"),
        actions: [
          PopupMenuButton<PopupMenuAction>(
              onSelected: _onPopupMenuSelected(mas),
              itemBuilder: (BuildContext context) =>
                  <PopupMenuEntry<PopupMenuAction>>[
                    const PopupMenuItem(
                        value: PopupMenuAction.add1, child: Text("+1")),
                    const PopupMenuItem(
                        value: PopupMenuAction.add10, child: Text("+10")),
                    const PopupMenuItem(
                        value: PopupMenuAction.add100, child: Text("+100")),
                    const PopupMenuDivider(),
                    const PopupMenuItem(
                        value: PopupMenuAction.exit, child: Text("exit")),
                  ])
        ],
      ),
      body: Center(
        child: Column(
          mainAxisAlignment: MainAxisAlignment.center,
          children: [
            const Text("You have pushed the button this many times:"),
            Obx(() => Text(
                  "${mas.counter}",
                  style: Theme.of(context).textTheme.displayLarge,
                ))
          ],
        ),
      ),
    );
  }
}
