import 'package:flutter/material.dart';
import 'package:get/get.dart';

void main() {
  runApp(GetMaterialApp(
    title: "Flutter Demo",
    theme: ThemeData(useMaterial3: true, primarySwatch: Colors.blue),
    home: const MainApp(),
  ));
}

class ListTileState extends GetxController {
  var index = 0.obs;
}

class IconLabel {
  IconLabel(this._icon, this._label);

  IconData get icon => _icon;
  String get label => _label;

  final IconData _icon;
  final String _label;
}

class MainApp extends StatelessWidget {
  const MainApp({super.key});

  @override
  Widget build(BuildContext context) {
    final ListTileState lts = Get.put(ListTileState());
    const textStyleNormal = TextStyle(
        color: Colors.black, fontSize: 18, fontWeight: FontWeight.normal);
    const textStyleSelected = TextStyle(
        color: Colors.black, fontSize: 18, fontWeight: FontWeight.bold);

    final textFormField = TextFormField(
      decoration: const InputDecoration(
          icon: Icon(Icons.format_size),
          hintText: "Font size",
          labelText: "Enter the font size"),
    );
    final historyTextFormField = TextFormField(
      decoration: const InputDecoration(
          icon: Icon(Icons.history), hintText: "Days", labelText: "Enter days"),
    );
    final languageTextFormField = TextFormField(
      decoration: const InputDecoration(
          icon: Icon(Icons.language),
          hintText: "Language",
          labelText: "Enter your language"),
    );
    var titleSelection = Obx(() {
      var selectitle = "Accessibility";
      if (lts.index.value == 0) {
        selectitle = "Accessibility";
      } else if (lts.index.value == 1) {
        selectitle = "History";
      } else {
        selectitle = "Language";
      }
      return Text("$selectitle Settings");
    });
    var textformSelection = Obx(() {
      if (lts.index.value == 0) {
        return textFormField;
      } else if (lts.index.value == 1) {
        return historyTextFormField;
      } else {
        return languageTextFormField;
      }
    });
    final icons = [
      IconLabel(Icons.accessibility, "Accessibility"),
      IconLabel(Icons.history, "History"),
      IconLabel(Icons.language, "Language")
    ];
    return Scaffold(
      appBar: AppBar(title: const Text("ListView & ListTile")),
      body: ListView(
        children: [
          for (var i = 0; i < icons.length; i++)
            Obx(() => ListTile(
                  leading: Icon(icons[i].icon),
                  title: Text(
                    icons[i].label,
                    style: lts.index.value != i
                        ? textStyleNormal
                        : textStyleSelected,
                  ),
                  subtitle: Text("${icons[i].label} Settings"),
                  trailing: const Icon(Icons.settings),
                  onTap: () => lts.index.value = i,
                ))
        ],
      ),
      bottomSheet: Container(
        color: const Color(0xffb3e5fc),
        padding: const EdgeInsets.all(20),
        child: Container(
          constraints: const BoxConstraints(maxHeight: 200),
          child: Column(children: [
            Row(
              mainAxisAlignment: MainAxisAlignment.center,
              children: [
                titleSelection,
                const Icon(Icons.settings),
              ],
            ),
            textformSelection
          ]),
        ),
      ),
    );
  }
}
