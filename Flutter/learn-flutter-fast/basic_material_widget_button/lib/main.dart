import 'package:flutter/material.dart';

void main() {
  runApp(const MainApp());
}

class MainApp extends StatelessWidget {
  const MainApp({super.key});

  @override
  Widget build(BuildContext context) {
    // var flatButtonRow = Row(
    //   mainAxisAlignment: MainAxisAlignment.center,
    //   children: [ButtonBar(
    //   )],);
    var elevatedButtonRow = Row(
      mainAxisAlignment: MainAxisAlignment.spaceEvenly,
      children: [
        ElevatedButton(
            onPressed: () => debugPrint("ElevatedButton pressed"),
            child: const Text("ElevatedButton")),
        const Text("ElevatedButton")
      ],
    );
    var iconButtonRow = Row(
      mainAxisAlignment: MainAxisAlignment.spaceEvenly,
      children: [
        IconButton(
            onPressed: () => debugPrint("IconButton pressed"),
            icon: const Icon(Icons.add)),
        const Text("IconButton")
      ],
    );
    var outlineButtonRow = Row(
      mainAxisAlignment: MainAxisAlignment.spaceEvenly,
      children: [
        OutlinedButton(
            onPressed: () => debugPrint("OutlineButton pressed"),
            child: const Text("OutlineButton")),
        const Text("IconButton")
      ],
    );
    var dropdownButtonRow = Row(
      mainAxisAlignment: MainAxisAlignment.spaceEvenly,
      children: [
        DropdownButton<String>(
          items: ["Men", "Women"]
              .map((String value) =>
                  DropdownMenuItem(value: value, child: Text(value)))
              .toList(),
          onChanged: (Object? value) => debugPrint("Changed: $value"),
        ),
        const Text("IconButton")
      ],
    );
    var backButtonRow = Row(
      mainAxisAlignment: MainAxisAlignment.spaceEvenly,
      children: [
        BackButton(
          onPressed: () => debugPrint("BackButton pressed"),
        ),
        const Text("BackButton")
      ],
    );
    var closeButtonRow = Row(
      mainAxisAlignment: MainAxisAlignment.spaceEvenly,
      children: [
        CloseButton(
          onPressed: () => debugPrint("CloseButton pressed"),
        ),
        const Text("CloseButton")
      ],
    );
    return MaterialApp(
      title: "Flutter Demo",
      theme: ThemeData(useMaterial3: true, primarySwatch: Colors.blue),
      home: Scaffold(
        body: Center(
          child: Column(
            mainAxisAlignment: MainAxisAlignment.spaceEvenly,
            children: [
              elevatedButtonRow,
              iconButtonRow,
              outlineButtonRow,
              dropdownButtonRow,
              backButtonRow,
              closeButtonRow
            ],
          ),
        ),
        floatingActionButton: FloatingActionButton(
            onPressed: () => debugPrint("FloatingActionButton pressed"),
            child: const Text("F.A.B")),
      ),
    );
  }
}
