import 'package:flutter/material.dart';
import 'package:get/get.dart';

void main() {
  runApp(MaterialApp(
    home: MainApp(),
    theme: ThemeData(useMaterial3: true, primarySwatch: Colors.blue),
    title: "Flutter Demo",
  ));
}

class SnackbarState extends GetxController {
  var isActive = false;
}

class MainApp extends StatelessWidget {
  MainApp({Key? key}) : super(key: key);

  final _scaffoldKey = GlobalKey<ScaffoldState>();

  @override
  Widget build(BuildContext context) {
    final SnackbarState sbs = Get.put(SnackbarState());
    return Scaffold(
      key: _scaffoldKey,
      body: const Center(
        child: Text("Press button to display snack bar"),
      ),
      floatingActionButton: FloatingActionButton.extended(
        icon: const Icon(Icons.explicit),
        label: const Text("Throw Error"),
        onPressed: () {
          if (sbs.isActive) return;
          sbs.isActive = true;
          ScaffoldMessenger.of(context)
              .showSnackBar(const SnackBar(
                content: Text("An unexpected error occurred: Error!"),
              ))
              .closed
              .then((value) => sbs.isActive = false);
        },
        tooltip: "Throw Error",
      ),
    );
  }
}
