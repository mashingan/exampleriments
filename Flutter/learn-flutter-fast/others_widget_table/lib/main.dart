import 'package:flutter/material.dart';

void main() {
  runApp(const MainApp());
}

class MainApp extends StatelessWidget {
  const MainApp({super.key});

  @override
  Widget build(BuildContext context) {
    const tableRow = TableRow(children: [
      Text(
        "aaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
        overflow: TextOverflow.fade,
      ),
      Text(
        "bbbbbbbbbbbbbbbbbbbbbbbbbbbbb",
        overflow: TextOverflow.clip,
      ),
      Text(
        "ccccccccccccccccccccccccccccc",
        overflow: TextOverflow.ellipsis,
      ),
    ]);
    return MaterialApp(
      title: "Flutter Demo",
      theme: ThemeData(useMaterial3: true, primarySwatch: Colors.blue),
      home: Scaffold(
        appBar: AppBar(title: const Text("Table")),
        body: Table(
          children: [for (var i = 0; i < 10; i++) tableRow],
          columnWidths: const {
            0: FlexColumnWidth(0.1),
            1: FlexColumnWidth(0.3),
            2: FlexColumnWidth(0.6),
          },
          border: TableBorder.all(),
        ),
      ),
    );
  }
}
