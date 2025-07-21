import 'package:flutter/material.dart';
import 'package:get/get.dart';

void main() {
  runApp(GetMaterialApp(
    title: "Flutter Demo",
    theme: ThemeData(useMaterial3: true, primarySwatch: Colors.blue),
    home: const MainApp(),
  ));
}

class PaddingState extends GetxController {
  final double _twenty = 20;
  var _index = 0.obs;
  var label = "".obs;
  late List<EdgeInsets> _edgeInserts;
  final _labels = [
    "All 20",
    "Left 20",
    "Right 20",
    "Top 20",
    "Bottom 20",
    "Sym horiz 20",
    "Sym vert 20"
  ];

  @override
  void onInit() {
    super.onInit();
    _edgeInserts = [
      EdgeInsets.all(_twenty),
      EdgeInsets.only(left: _twenty),
      EdgeInsets.only(right: _twenty),
      EdgeInsets.only(top: _twenty),
      EdgeInsets.only(bottom: _twenty),
      EdgeInsets.symmetric(horizontal: _twenty),
      EdgeInsets.symmetric(vertical: _twenty)
    ];
    label.value = _toLabel();
  }

  String _toLabel() => _labels[_index.value].toString();
  next() {
    _index++;
    if (_index >= _edgeInserts.length) _index.value = 0;
    label.value = _toLabel();
  }
}

class MainApp extends StatelessWidget {
  const MainApp({super.key});

  @override
  Widget build(BuildContext context) {
    final PaddingState ps = Get.put(PaddingState());
    return Scaffold(
      appBar: AppBar(
        title: Obx(() => Text("${ps.label}")),
        actions: [
          IconButton(onPressed: ps.next, icon: const Icon(Icons.refresh))
        ],
      ),
      body: Center(
          child: Container(
        decoration: BoxDecoration(border: Border.all(color: Colors.blueAccent)),
        child: Obx(() => Padding(
              padding: ps._edgeInserts[ps._index.value],
              child: Container(color: Colors.blue),
            )),
      )),
    );
  }
}
