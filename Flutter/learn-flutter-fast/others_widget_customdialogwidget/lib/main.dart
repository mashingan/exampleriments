import 'package:flutter/material.dart';
import 'package:get/get.dart';

void main() {
  runApp(const GetMaterialApp(
    home: MainApp(),
  ));
}

class MainAppState extends GetxController {
  var gridoptions = GridOptions(2, 3, 1.0, 4.0, 4.0).obs;
  set options(GridOptions gd) => gridoptions.value = gd;
}

class GridOptions extends GetxController {
  final Rx<int> _crossAxisCountPortrait;
  final Rx<int> _crossAxisCountLandscape;
  final Rx<double> _childAspectRatio;
  final Rx<double> _padding;
  final Rx<double> _spacing;

  GridOptions(int crossAxisCountPortrait, int crossAxisCountLandscape,
      double childAspectRatio, double padding, double spacing)
      : _crossAxisCountPortrait = crossAxisCountPortrait.obs,
        _crossAxisCountLandscape = crossAxisCountLandscape.obs,
        _childAspectRatio = childAspectRatio.obs,
        _padding = padding.obs,
        _spacing = spacing.obs;

  GridOptions.from(GridOptions gdop)
      : _crossAxisCountPortrait = gdop._crossAxisCountPortrait,
        _crossAxisCountLandscape = gdop._crossAxisCountLandscape,
        _childAspectRatio = gdop._childAspectRatio,
        _padding = gdop._padding,
        _spacing = gdop._spacing;

  // @override
  // void onInit() {
  //   super.onInit();
  //   _crossAxisCountPortrait = 2.obs;
  //   _crossAxisCountLandscape = 3.obs;
  //   _childAspectRatio = 1.0.obs;
  //   _padding = 4.0.obs;
  //   _spacing = 4.0.obs;
  // }

  @override
  String toString() {
    return "GridOptions{\n\t_crossAxisCountPortrait: ${_crossAxisCountPortrait.value},\n\t_crossAxisCountLandscape: ${_crossAxisCountLandscape.value},\n\t_childAspectRatio: ${_childAspectRatio.value},\n\t_padding: ${_padding.value},\n\t_spacing: ${_spacing.value}}";
  }
}

class CustomDialogWidget extends StatelessWidget {
  const CustomDialogWidget({super.key});

  @override
  Widget build(BuildContext context) {
    final GridOptions gdop = Get.find();
    return Container(
      constraints: const BoxConstraints(maxHeight: 400, maxWidth: 250),
      // height: 400,
      // width: 250,
      child: Column(
        mainAxisAlignment: MainAxisAlignment.spaceAround,
        children: [
          const Text("Grid Options",
              style: TextStyle(fontSize: 20, fontWeight: FontWeight.bold)),
          Row(
            mainAxisAlignment: MainAxisAlignment.center,
            children: [
              const Spacer(),
              const Text("Cross Axis Count Portrait"),
              Obx(() => DropdownButton<int>(
                  value: gdop._crossAxisCountPortrait.value,
                  items: [2, 3, 4, 5, 6].map((int value) {
                    return DropdownMenuItem<int>(
                      value: value,
                      child: Text("$value"),
                    );
                  }).toList(),
                  onChanged: (newValue) {
                    debugPrint("Axis portrait newvalue: $newValue");
                    gdop._crossAxisCountPortrait.value = newValue!;
                  })),
              const Spacer(),
            ],
          ),
          Row(
            mainAxisAlignment: MainAxisAlignment.center,
            children: [
              const Spacer(),
              const Text("Cross Axis Count Landscape"),
              Obx(() => DropdownButton<int>(
                  value: gdop._crossAxisCountLandscape.value,
                  items: [2, 3, 4, 5, 6].map((int value) {
                    return DropdownMenuItem<int>(
                      value: value,
                      child: Text("$value"),
                    );
                  }).toList(),
                  onChanged: (newValue) {
                    debugPrint("Axis landscape newvalue: $newValue");
                    gdop._crossAxisCountLandscape.value = newValue!;
                  })),
              const Spacer(),
            ],
          ),
          Row(
            mainAxisAlignment: MainAxisAlignment.center,
            children: [
              const Spacer(),
              const Text("Aspect Ratio"),
              Obx(() => DropdownButton<double>(
                    value: gdop._childAspectRatio.value,
                    items: [1.0, 1.5, 2.0, 2.5].map((double value) {
                      return DropdownMenuItem<double>(
                        value: value,
                        child: Text("$value"),
                      );
                    }).toList(),
                    onChanged: (newValue) =>
                        gdop._childAspectRatio.value = newValue!,
                  )),
              const Spacer(),
            ],
          ),
          Row(
            mainAxisAlignment: MainAxisAlignment.center,
            children: [
              const Spacer(),
              const Text("Padding"),
              Obx(() => DropdownButton<double>(
                    value: gdop._padding.value,
                    items: [1.0, 2.0, 4.0, 8.0, 16.0, 32.0].map((double value) {
                      return DropdownMenuItem<double>(
                        value: value,
                        child: Text("$value"),
                      );
                    }).toList(),
                    onChanged: (newValue) => gdop._padding.value = newValue!,
                  )),
              const Spacer(),
            ],
          ),
          Row(
            mainAxisAlignment: MainAxisAlignment.center,
            children: [
              const Spacer(),
              const Text("Spacing"),
              Obx(() => DropdownButton<double>(
                    value: gdop._spacing.value,
                    items: [1.0, 2.0, 4.0, 8.0, 16.0, 32.0].map((double value) {
                      return DropdownMenuItem<double>(
                        value: value,
                        child: Text("$value"),
                      );
                    }).toList(),
                    onChanged: (newValue) => gdop._spacing.value = newValue!,
                  )),
              const Spacer(),
            ],
          ),
          TextButton(
              onPressed: () => Get.back(result: gdop),
              child: const Text("Apply"))
        ],
      ),
    );
  }
}

class MainApp extends StatelessWidget {
  const MainApp({super.key});

  _showGridOptionDialog(BuildContext context) async {
    await showDialog<GridOptions>(
        context: context,
        builder: (BuildContext context) {
          return const Dialog(
            child: CustomDialogWidget(),
          );
        });
  }

  @override
  Widget build(BuildContext context) {
    final GridOptions gdop = Get.put(GridOptions(2, 3, 1, 4, 4));
    var kittenTiles = <Widget>[];
    for (int i = 200; i < 1000; i += 100) {
      String imageuri = "http://placekitten.com/200/$i";
      kittenTiles.add(GridTile(
          header: const GridTileBar(
            title: Text(
              "Cats",
              style: TextStyle(fontWeight: FontWeight.bold),
            ),
            backgroundColor: Color.fromRGBO(0, 0, 0, 0.5),
          ),
          footer: const GridTileBar(
              title: Text(
            "How cute",
            textAlign: TextAlign.right,
            style: TextStyle(fontWeight: FontWeight.bold),
          )),
          child: Image.network(
            imageuri,
            fit: BoxFit.cover,
          )));
    }
    return Scaffold(
      appBar: AppBar(title: const Text("Grid View")),
      body: OrientationBuilder(builder: (context, orientation) {
        return Obx(() => GridView.count(
              crossAxisCount: (orientation == Orientation.portrait)
                  ? gdop._crossAxisCountPortrait.value
                  : gdop._crossAxisCountLandscape.value,
              childAspectRatio: gdop._childAspectRatio.value,
              padding: EdgeInsets.all(gdop._padding.value),
              mainAxisSpacing: gdop._spacing.value,
              crossAxisSpacing: gdop._spacing.value,
              children: kittenTiles,
            ));
      }),
      floatingActionButton: FloatingActionButton(
        onPressed: () => _showGridOptionDialog(context),
        tooltip: "Try more grid options",
        child: const Icon(Icons.refresh),
      ),
    );
  }
}
