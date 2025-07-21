import 'package:flutter/material.dart';
import 'package:get/get.dart';

void main() {
  runApp(GetMaterialApp(
    title: "Flutter Demo",
    theme: ThemeData(useMaterial3: true, primarySwatch: Colors.blue),
    home: const MainApp(),
  ));
}

class FlexState extends GetxController {
  var alignment = [
    MainAxisAlignment.center,
    MainAxisAlignment.end,
    MainAxisAlignment.spaceAround,
    MainAxisAlignment.spaceEvenly,
    MainAxisAlignment.spaceBetween,
    MainAxisAlignment.start
  ].map((e) => e.obs);
  late List<String> _alignmentLabel;
  late Rx<String> currentLabel;

  @override
  void onInit() {
    super.onInit();
    _alignmentLabel =
        alignment.map((e) => e.value.toString().split('.')[1]).toList();
    _alignmentIndex.value = 0;
    currentLabel = _alignmentLabel[0].obs;
  }
  var _alignmentIndex = 0.obs;
  var vertical = true.obs;
  var actionText = "Vertical".obs;
  var rgbButton = [
    Colors.red,
    Colors.green,
    Colors.blue
  ].map((e) => RawMaterialButton(elevation: 2, fillColor: e, onPressed: () {}));
  toggle() {
    vertical.toggle();
    if (vertical.isTrue) {
      actionText.value = "Vertical";
    } else {
      actionText.value = "Horizontal";
    }
  }

  next() {
    debugPrint("next is pressed");
    _alignmentIndex++;
    if (_alignmentIndex >= alignment.length) {
      _alignmentIndex.value = 0;
    }
    currentLabel.value = _alignmentLabel[_alignmentIndex.value];
  }
}

class MainApp extends StatelessWidget {
  const MainApp({super.key});

  @override
  Widget build(BuildContext context) {
    final FlexState fs = Get.put(FlexState());
    return Scaffold(
        appBar: AppBar(
          title: const Text("Flex"),
          actions: [
            IconButton(
                onPressed: fs.toggle,
                icon: const Icon(Icons.rotate_right)),
            Padding(
                padding: const EdgeInsets.only(top: 20),
                child: Obx(() => Text("${fs.actionText}"))),
            IconButton(
                tooltip: "Main axis",
                onPressed: fs.next,
                icon: const Icon(Icons.aspect_ratio)),
            Padding(
              padding: const EdgeInsets.only(top: 20),
              child:
                  Obx(() => Text("${fs.currentLabel}"))
            ),
            const Padding(padding: EdgeInsets.all(10))
          ],
        ),
        body: Obx(() => Flex(
            direction: fs.vertical.isTrue ? Axis.vertical : Axis.horizontal,
            mainAxisAlignment: fs.alignment.elementAt(fs._alignmentIndex.value).value,
            children: fs.rgbButton.toList())));
  }
}
