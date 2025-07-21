import 'dart:ui';

import 'package:flutter/material.dart';

void main() {
  runApp(AppWidget());
}

class AppWidget extends StatefulWidget {
  AppWidget({super.key}) {
    debugPrint("AppWidget - constructor - ${hashCode.toString()}");
  }

  @override
  _AppWidgetState createState() {
    debugPrint("AppWidget - createState - ${hashCode.toString()}");
    return _AppWidgetState();
  }
}

const _imageClear =
    "https://upload.wikimedia.org/wikipedia/commons/9/9f/LiliumAuratumVVirginaleBluete2Rework.jpg";
const _imageBlur =
    "https://images.unsplash.com/photo-1531603071569-0dd65ad72d53?ixlib=rb-1.2.1&ixid=eyJhcHBfaWQiOjEyMDd9&w=1000&q=80";

class _AppWidgetState extends State<AppWidget> {
  bool _bright = false;
  void _brightness() {
    setState(() => _bright = !_bright);
  }

  @override
  Widget build(BuildContext context) {
    debugPrint("_AppWidgetState - build - ${hashCode.toString()}");
    return MaterialApp(
        title: "Flutter Demo",
        theme: ThemeData(
          primarySwatch: Colors.blue,
          brightness: _bright ? Brightness.light : Brightness.dark,
        ),
        home: FlowerWidget(
            imageSrc: _bright ? _imageClear : _imageBlur,
            brightnessCallback: _brightness));
  }
}

class FlowerWidget extends StatefulWidget {
  final String? imageSrc;
  final VoidCallback? brightnessCallback;

  FlowerWidget({Key? key, this.imageSrc, this.brightnessCallback})
      : super(key: key) {
    debugPrint("FlowerWidget - constructor - ${hashCode.toString()}");
  }
  @override
  State<FlowerWidget> createState() {
    debugPrint("FlowerWidget - createState - ${hashCode.toString()}");
    return _FlowerWidgetState();
  }
}

class _FlowerWidgetState extends State<FlowerWidget> {
  double _blur = 0;

  _FlowerWidgetState() {
    debugPrint("_FlowerWidgetState - constructor - ${hashCode.toString()}");
  }

  @override
  void initState() {
    super.initState();
    debugPrint("_FlowerWidgetState - initState - ${hashCode.toString()}");
  }

  @override
  void didChangeDependencies() {
    super.didChangeDependencies();
    debugPrint(
        "_FlowerWidgetState - didChangeDependencies - ${hashCode.toString()}");
  }

  @override
  void didUpdateWidget(Widget oldWidget) {
    super.didUpdateWidget(oldWidget as FlowerWidget);
    debugPrint("_FlowerWidgetState - didUpdateWidget - ${hashCode.toString()}");
    _blur = 0;
  }

  void _blurMore() {
    setState(
      () => _blur += 5.0,
    );
  }

  @override
  Widget build(BuildContext context) {
    debugPrint("_FlowerWidgetState - build - ${hashCode.toString()}");
    return Scaffold(
      appBar: AppBar(
        title: const Text("Flower"),
        actions: [
          IconButton(
              onPressed: widget.brightnessCallback,
              icon: const Icon(Icons.refresh))
        ],
      ),
      body: Container(
        decoration: BoxDecoration(
            color: Theme.of(context).colorScheme.background,
            image: DecorationImage(
              image: NetworkImage(widget.imageSrc!),
              fit: BoxFit.cover,
            )),
        child: BackdropFilter(
          filter: ImageFilter.blur(sigmaX: _blur, sigmaY: _blur),
          child: Container(
            decoration: BoxDecoration(
              color: Colors.white.withOpacity(0),
            ),
          ),
        ),
      ),
      floatingActionButton: FloatingActionButton(
        onPressed: _blurMore,
        tooltip: "Blur more",
        child: const Icon(Icons.add),
      ),
    );
  }
}
