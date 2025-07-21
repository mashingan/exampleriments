import 'dart:math';

import 'package:flutter/material.dart';
import 'package:get/get.dart';

void main() {
  runApp(
    GetMaterialApp(
        home: MainApp(),
        theme: ThemeData(useMaterial3: true, primarySwatch: Colors.blue)),
  );
}

class Cat {
  String imageSrc;
  String name;
  int age;
  int votes;

  Cat(this.imageSrc, this.name, this.age, this.votes);

  @override
  operator ==(other) => (other is Cat) && (imageSrc == other.imageSrc);

  @override
  int get hashCode => imageSrc.hashCode;
}

class MainApp extends StatelessWidget {
  MainApp({super.key}) {
    for (var i = 200; i < 250; i += 10) {
      _cats.add(Cat("http://placekitten.com/200/$i",
          _catNames[next(0, _catNames.length - 1)], next(1, 32), 0));
    }
  }

  // final GlobalKey<AnimatedListState> _listkey = GlobalKey();
  final _catNames = [
    "Tom",
    "Oliver",
    "Ginger",
    "Pontouf",
    "Madison",
    "Bubblita",
    "Bubbles"
  ];
  final _random = Random();
  final _cats = <Cat>[];

  int next(int min, int max) => min + _random.nextInt(max - min);

  Widget _buildItem(Cat cat, {int index = -1}) {
    return ListTile(
      key: Key("ListTile:${cat.hashCode.toString()}"),
      leading: CircleAvatar(
        backgroundImage: NetworkImage(cat.imageSrc),
        radius: 32,
      ),
      title: Text(cat.name, style: const TextStyle(fontSize: 25)),
      subtitle: Text(
        "Age: ${cat.age} year${cat.age > 1 ? 's' : ''} old.",
        style: const TextStyle(fontSize: 25),
      ),
    );
  }

  Future<bool?> Function(DismissDirection) _confirmDismiss(
      BuildContext context) {
    return (DismissDirection direction) async {
      return await showDialog<bool>(
          context: context,
          builder: (BuildContext context) {
            return AlertDialog(
              title: const Text("Confirm"),
              content: const Text(
                  "Are you sure want to delete this cat?\n\nHe is cute you know..."),
              actions: [
                TextButton(
                    onPressed: () => Get.back(result: true),
                    child: const Text("Yes")),
                TextButton(
                    onPressed: () => Get.back(result: false),
                    child: const Text("No"))
              ],
            );
          });
    };
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(title: const Text("The Cats List")),
      body: ListView.builder(
          itemCount: _cats.length,
          itemBuilder: (context, index) {
            Cat cat = _cats[index];
            return Dismissible(
                confirmDismiss: _confirmDismiss(context),
                key: ValueKey(cat.hashCode.toString()),
                child: _buildItem(cat, index: index));
          }),
    );
  }
}
