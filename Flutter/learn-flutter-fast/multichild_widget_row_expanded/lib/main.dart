import 'package:flutter/material.dart';

void main() {
  runApp(const MainApp());
}

class MainApp extends StatelessWidget {
  const MainApp({super.key});

  @override
  Widget build(BuildContext context) {
    final texts = ["aaaaaaaaaa", "bbbbbbbbbb", "cccccccccc"];
    return MaterialApp(
        title: "Flutter Demo",
        theme: ThemeData(
          useMaterial3: true,
          primarySwatch: Colors.blue,
        ),
        home: Scaffold(
          appBar: AppBar(
            title: const Text("Rows"),
          ),
          body: Column(
              mainAxisAlignment: MainAxisAlignment.spaceEvenly,
              children: [
                const Text("None expanded:"),
                Row(
                  mainAxisAlignment: MainAxisAlignment.center,
                  children: [for (var t in texts) Text(t)],
                ),
                const Text("1st child expanded:"),
                Row(mainAxisAlignment: MainAxisAlignment.center, children: [
                  Expanded(
                    child: Text(texts[0]),
                  ),
                  for (var i = 1; i < texts.length; i++) Text(texts[i])
                ]),
                const Text("2nd child expanded:"),
                Row(mainAxisAlignment: MainAxisAlignment.center, children: [
                  Text(texts[0]),
                  Expanded(child: Text(texts[1])),
                  Text(texts[2]),
                ]),
                const Text("3rd child expanded:"),
                Row(mainAxisAlignment: MainAxisAlignment.center, children: [
                  for (var i = 0; i < texts.length-1; i++) Text(texts[i]),
                  Expanded(child: Text(texts[2])),
                ]),
              ]),
        ));
  }
}
