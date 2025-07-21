import 'package:flutter/material.dart';
import 'package:get/get.dart';

void main() {
  runApp(GetMaterialApp(
    home: const MainApp(),
    title: "Flutter Demo",
    theme: ThemeData(useMaterial3: true, primarySwatch: Colors.blue),
  ));
}

class GridOptions {
  final Rx<int> _crossAxisCountPortrait;
  final Rx<int> _crossAxisCountLandscape;
  final Rx<double> _childAspectRatio;
  final Rx<double> _padding;
  final Rx<double> _spacing;
  var labelNavbar = "".obs;

  var _gridIndex = -1;

  GridOptions(int crossAxisCountPortrait, int crossAxisCountLandscape,
      double childAspectRatio, double padding, double spacing)
      : _crossAxisCountPortrait = crossAxisCountPortrait.obs,
        _crossAxisCountLandscape = crossAxisCountLandscape.obs,
        _childAspectRatio = childAspectRatio.obs,
        _padding = padding.obs,
        _spacing = spacing.obs;

  int get crossAxisCountPortrait => _crossAxisCountPortrait.value;
  int get crossAxisCountLandscape => _crossAxisCountLandscape.value;
  double get childAspectRatio => _childAspectRatio.value;
  double get padding => _padding.value;
  double get spacing => _spacing.value;
  set crossAxisCountPortrait(int value) =>
      _crossAxisCountPortrait.value = value;
  set crossAxisCountLandscape(int value) =>
      _crossAxisCountLandscape.value = value;
  set childAspectRatio(double value) => _childAspectRatio.value = value;
  set padding(double value) => _padding.value = value;
  set spacing(double value) => _spacing.value = value;

  _newSetting(_GridOptionData data) {
    _crossAxisCountPortrait.value = data.portrait;
    _crossAxisCountLandscape.value = data.landscape;
    _childAspectRatio.value = data.aspectRatio;
    _padding.value = data.padding;
    _spacing.value = data.spacing;
    labelNavbar.value = toString();
  }

  final _options = [
    _GridOptionData(2, 3, 1.0, 10.0, 10.0),
    _GridOptionData(3, 4, 1.0, 10.0, 10.0),
    _GridOptionData(4, 5, 1.0, 10.0, 10.0),
    _GridOptionData(2, 3, 1.0, 10.0, 10.0),
    _GridOptionData(2, 3, 1.5, 10.0, 10.0),
    _GridOptionData(2, 3, 2.0, 10.0, 10.0),
    _GridOptionData(2, 3, 1.0, 10.0, 10.0),
    _GridOptionData(2, 3, 1.5, 20.0, 10.0),
    _GridOptionData(2, 3, 2.0, 30.0, 10.0),
    _GridOptionData(2, 3, 1.0, 10.0, 10.0),
    _GridOptionData(2, 3, 1.5, 10.0, 20.0),
    _GridOptionData(2, 3, 2.0, 10.0, 30.0),
  ];

  next() {
    _gridIndex++;
    _gridIndex %= _options.length;
    _newSetting(_options[_gridIndex]);
  }

  @override
  String toString() {
    return "GridView{crossAxisCountPortrait: ${_crossAxisCountPortrait.value}, "
        "crossAxisCountLandscape: ${_crossAxisCountLandscape.value}, "
        "childAspectRatio: ${_childAspectRatio.value}, "
        "padding: ${_padding.value}, spacing: ${_spacing.value}}";
  }
}

class _GridOptionData {
  final int portrait;
  final int landscape;
  final double aspectRatio;
  final double padding;
  final double spacing;

  _GridOptionData(this.portrait, this.landscape, this.aspectRatio, this.padding,
      this.spacing);
}

class MainApp extends StatelessWidget {
  const MainApp({super.key});

  @override
  Widget build(BuildContext context) {
    final GridOptions gd = Get.put(GridOptions(2, 3, 1.0, 4.0, 4.0));
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
                  ? gd.crossAxisCountPortrait
                  : gd.crossAxisCountLandscape,
              childAspectRatio: gd.childAspectRatio,
              padding: EdgeInsets.all(gd.padding),
              mainAxisSpacing: gd.spacing,
              crossAxisSpacing: gd.spacing,
              children: kittenTiles,
            ));
      }),
      bottomNavigationBar: Container(
        padding: const EdgeInsets.all(20),
        child: Obx(() => Text("${gd.labelNavbar}")),
      ),
      floatingActionButton: FloatingActionButton(
          onPressed: gd.next,
          tooltip: "Try more grid options",
          child: const Icon(Icons.refresh)),
    );
  }
}
