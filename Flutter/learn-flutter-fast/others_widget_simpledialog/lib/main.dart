import 'package:flutter/material.dart';
import 'package:get/get.dart';

void main() {
  runApp(const GetMaterialApp(
    home: MainApp(),
  ));
}

class MainAppState extends GetxController {
  final _boxfit = BoxFit.cover.obs;
  set boxfit(BoxFit bf) => _boxfit.value = bf;
  BoxFit get boxfit => _boxfit.value;
}

class MainApp extends StatelessWidget {
  const MainApp({super.key});

  _showBoxFitDialog(BuildContext context, MainAppState mas) async {
    var bxsels = [
      BoxFit.cover,
      BoxFit.contain,
      BoxFit.fill,
      BoxFit.fitHeight,
      BoxFit.fitWidth,
      BoxFit.scaleDown,
      BoxFit.none,
    ];
    var bxtext = [
      "Cover",
      "Contain",
      "Fill",
      "Fit height",
      "Fit width",
      "Scale down",
      "None"
    ];
    var boxfit = await showDialog<BoxFit>(
        context: context,
        builder: (BuildContext context) {
          return SimpleDialog(
            title: const Text("Select Box Fit"),
            children: [
              for (var i = 0; i < bxsels.length; i++)
                SimpleDialogOption(
                  onPressed: () => Get.back(result: bxsels[i]),
                  child: Text(bxtext[i]),
                )
            ],
          );
        });

    if (boxfit != null) mas.boxfit = boxfit;
  }

  @override
  Widget build(BuildContext context) {
    final MainAppState mas = Get.put(MainAppState());
    var kittenTiles = <Widget>[];
    for (int i = 200; i < 1000; i += 100) {
      String imageuri = "http://placekitten.com/200/$i";
      kittenTiles.add(Obx(
        () => GridTile(
            child: Image.network(
          imageuri,
          fit: mas.boxfit,
        )),
      ));
    }
    return Scaffold(
      appBar: AppBar(title: Obx(() => Text("Simple Dialog: ${mas.boxfit}"))),
      body: OrientationBuilder(builder: (context, orientation) {
        return GridView.count(
          crossAxisCount: (orientation == Orientation.portrait) ? 2 : 3,
          childAspectRatio: 1,
          mainAxisSpacing: 1,
          crossAxisSpacing: 1,
          children: kittenTiles,
        );
      }),
      floatingActionButton: FloatingActionButton(
        onPressed: () => _showBoxFitDialog(context, mas),
        child: const Icon(Icons.select_all),
      ),
    );
  }
}
