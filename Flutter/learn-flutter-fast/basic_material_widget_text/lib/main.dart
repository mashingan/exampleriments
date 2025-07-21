import 'package:flutter/material.dart';
import 'package:get/get.dart';

void main() {
  runApp(GetMaterialApp(
    title: "Flutter Styled Text Demo",
    home: MyApp(),
    theme: ThemeData(primarySwatch: Colors.blue),
  ));
}

class TextBlock {
  final Color _color;
  final String _text;
  TextBlock(this._color, this._text);
  String get text => _text;
  Color get color => _color;
}

class AppState extends GetxController {
  var index = -1;
  final _textBlocks = <TextBlock>[
    TextBlock(Colors.red, "every"),
    TextBlock(Colors.redAccent, " schoolboy"),
    TextBlock(Colors.green, "\nknows"),
    TextBlock(Colors.greenAccent, " who"),
    TextBlock(Colors.blue, "\nimprisoned"),
    TextBlock(Colors.blueAccent, "\nMontezuma"),
  ];
  var texts = <TextSpan>[].obs;


  void nextCounter() {
    index++;
    if (index > _textBlocks.length || index % _textBlocks.length == 0) {
      index = 0;
      texts.clear();
    }
    texts.add(TextSpan(
        text: _textBlocks[index].text,
        style: TextStyle(color: _textBlocks[index].color, fontSize: 32)));
  }
}

class MyApp extends StatelessWidget {
  @override
  Widget build(BuildContext context) {
    final AppState aps = Get.put(AppState());
    return Scaffold(
      body: Center(
        child: Column(
          mainAxisAlignment: MainAxisAlignment.center,
          children: [Obx(() => Text.rich(TextSpan(children: aps.texts.toList())))],
        ),
      ),
      floatingActionButton: FloatingActionButton(
        onPressed: aps.nextCounter,
        tooltip: "Increment",
        child: const Icon(Icons.note_add),
      ),
    );
  }
}
