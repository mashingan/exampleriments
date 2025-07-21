import 'package:flutter/material.dart';

void main() {
  runApp(const MainApp());
}

class MainApp extends StatelessWidget {
  const MainApp({super.key});

  @override
  Widget build(BuildContext context) {
    return const MaterialApp(
      home: FoldScaf(),
    );
  }
}

class FoldScaf extends StatelessWidget {
  const FoldScaf({super.key});
  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: const Text("AppBar"),
        backgroundColor: Colors.amber,
        actions: [
          IconButton(
              onPressed: () => debugPrint("Add IconButton pressed..."),
              icon: const Icon(Icons.add))
        ],
      ),
      backgroundColor: Colors.lightBlueAccent,
      body: const Center(
        child: Column(
          mainAxisAlignment: MainAxisAlignment.center,
          children: [Text("Body")],
        ),
      ),
      bottomNavigationBar: BottomNavigationBar(
        type: BottomNavigationBarType.fixed,
        onTap: (index) => debugPrint("BottomNavigationBar onTap: $index"),
        items: const [
          BottomNavigationBarItem(
              icon: Icon(Icons.home), label: "Bottom nav bar item 1"),
          BottomNavigationBarItem(
              icon: Icon(Icons.mail), label: "Bottom nav bar item 2"),
        ],
      ),
      bottomSheet: Container(
        color: Colors.amberAccent,
        padding: const EdgeInsets.all(20),
        child: Row(children: [
          IconButton(
              onPressed: () => debugPrint("Bottom sheet icon pressed..."),
              icon: const Icon(Icons.update)),
          const Text("Bottom sheet")
        ]),
      ),
      drawer: Drawer(
        child: ListView(
          children: [
            Row(
              children: [
                IconButton(
                    onPressed: () => debugPrint("Drawer item 1 pressed..."),
                    icon: const Icon(Icons.add)),
                const Text("Drawer item 1")
              ],
            ),
            Row(
              children: [
                IconButton(
                    onPressed: () => debugPrint("Drawer item 2 pressed..."),
                    icon: const Icon(Icons.add)),
                const Text("Drawer item 2")
              ],
            )
          ],
        ),
      ),
      endDrawer: Drawer(
        child: ListView(children: [
          Row(
            children: [
              IconButton(
                icon: const Icon(Icons.add),
                onPressed: () => debugPrint("Drawer item 1 pressed..."),
              ),
              const Text("Drawer Item 1")
            ],
          ),
          Row(
            children: [
              IconButton(
                icon: const Icon(Icons.add),
                onPressed: () => debugPrint("Drawer item 2 pressed..."),
              ),
              const Text("Drawer Item 2")
            ],
          )
        ]),
      ),
      floatingActionButton: FloatingActionButton(
        onPressed: () => debugPrint("FAB pressed"),
        tooltip: 'Increment',
        child: const Icon(Icons.add),
      ),
      persistentFooterButtons: [
        IconButton(
            onPressed: () => debugPrint("Persistent footer icon pressed"),
            icon: const Icon(Icons.update)),
        const Text("Persisten footer")
      ],
    );
  }
}
