import 'package:flutter/material.dart';

void main() {
  runApp(const MainApp());
}

class Tab1 extends StatelessWidget {
  const Tab1({super.key});
  @override
  Widget build(BuildContext context) {
    return Image.network("https://cdn2.thecatapi.com/images/MTY1NDA3OA.jpg");
  }
}

class Tab2 extends StatelessWidget {
  const Tab2({super.key});
  @override
  Widget build(BuildContext context) {
    return Image.network("https://cdn2.thecatapi.com/images/68j.jpg");
  }
}

class Tab3 extends StatelessWidget {
  const Tab3({super.key});
  @override
  Widget build(BuildContext context) {
    return Image.network("https://cdn2.thecatapi.com/images/ece.jpg");
  }
}

class MainApp extends StatelessWidget {
  const MainApp({super.key});

  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      home: DefaultTabController(
          length: 3,
          child: Scaffold(
            appBar: AppBar(title: const Text("Cat Tabs")),
            body: const TabBarView(
              children: [Tab1(), Tab2(), Tab3()],
            ),
            bottomNavigationBar: const TabBar(
              labelColor: Colors.blue,
              unselectedLabelColor: Colors.grey,
              labelStyle: TextStyle(
                color: Colors.blue, fontWeight: FontWeight.w800),
              indicatorColor: Colors.white,
              tabs: [
                Tab(text: "Cat #1", icon: Icon(Icons.keyboard_arrow_left)),
                Tab(text: "Cat #2", icon: Icon(Icons.keyboard_arrow_up)),
                Tab(text: "Cat #3", icon: Icon(Icons.keyboard_arrow_right)),
              ],
            ),
          )),
    );
  }
}
