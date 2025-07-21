import 'package:flutter/material.dart';

void main() {
  runApp(const MainApp());
}

class MainApp extends StatelessWidget {
  const MainApp({super.key});

  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      title: "Flutter Demo",
      theme: ThemeData(useMaterial3: true, primarySwatch: Colors.blue),
      home: const MyHomePage(
        title: "Flutter list containered box",
      ),
    );
  }
}

class MyHomePage extends StatefulWidget {
  const MyHomePage({Key? key, this.title}) : super(key: key);

  final String? title;

  @override
  State<MyHomePage> createState() => _MyHomePageState();
}

class _MyHomePageState extends State<MyHomePage> {
  int _counter = 0;
  final _controller = ScrollController();

  void _incCount() => setState(() {
        _counter++;
        _scrollDown();
      });

  _scrollDown() {
    _controller.jumpTo(_controller.position.maxScrollExtent);
  }

  _scrollTop() {
    _controller.animateTo(_controller.position.minScrollExtent,
        duration: const Duration(seconds: 1), curve: Curves.fastOutSlowIn);
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(title: Text(widget.title!)),
      body: Center(
          child: ConstrainedBox(
        constraints: const BoxConstraints(
            maxHeight: 350, minHeight: 300, minWidth: 200, maxWidth: 250),
        child: Container(
          decoration: BoxDecoration(border: Border.all()),
          padding: const EdgeInsets.all(5),
          child: ListView.builder(
            controller: _controller,
            itemCount: _counter,
            itemBuilder: (_, i) => Text("Row ${i + 1}"),
          ),
        ),
      )),
      floatingActionButton:
          Row(mainAxisAlignment: MainAxisAlignment.end, children: [
        FloatingActionButton(
          onPressed: _scrollTop,
          tooltip: "Top",
          child: const Icon(Icons.arrow_upward),
        ),
        const SizedBox(
          width: 5,
        ),
        FloatingActionButton(
          onPressed: _incCount,
          tooltip: "Increment",
          child: const Icon(Icons.add),
        ),
        const SizedBox(
          width: 5,
        ),
        FloatingActionButton(
          onPressed: () => setState(() {
            _counter = 0;
          }),
          tooltip: "Reset",
          child: const Icon(Icons.refresh),
        )
      ]),
    );
  }
}
