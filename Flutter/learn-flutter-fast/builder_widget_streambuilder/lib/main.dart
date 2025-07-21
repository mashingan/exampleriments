import 'package:builder_widget_streambuilder/bloc.dart';
import 'package:builder_widget_streambuilder/square.dart';
import 'package:flutter/material.dart';

void main() {
  runApp(MainApp());
}

class MainApp extends StatelessWidget {
  MainApp({super.key});

  final _bloc = Bloc();

  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      title: "Flutter Demo Nested Builders",
      theme: ThemeData(primarySwatch: Colors.blue, useMaterial3: true),
      home: BlocProvider(
        bloc: _bloc,
        child: const HomeWidget(title: "Nested builders"),
      ),
    );
  }
}

class HomeWidget extends StatelessWidget {
  final String title;
  const HomeWidget({Key? key, required this.title}) : super(key: key);

  @override
  Widget build(BuildContext context) {
    final bloc = BlocProvider.of(context);
    return Scaffold(
      appBar: AppBar(title: const Text("Nested builders")),
      body: StreamBuilder<List<Square>>(
        stream: bloc.squareListStream,
        initialData: bloc.initSquareList(),
        builder: (context, snapshot) {
          var squares = snapshot.data;
          return OrientationBuilder(builder: (context, orientation) {
            return GridView.builder(
                itemCount: squares?.length,
                gridDelegate: SliverGridDelegateWithFixedCrossAxisCount(
                    crossAxisCount:
                        (orientation == Orientation.portrait ? 3 : 4)),
                itemBuilder: ((context, index) => GridTile(
                        child: Container(
                      color: squares?[index].color,
                      child: Padding(
                        padding: const EdgeInsets.all(20),
                        child: Text(
                          squares![index].text,
                          style: const TextStyle(
                              fontSize: 20, fontWeight: FontWeight.bold),
                          textAlign: TextAlign.center,
                        ),
                      ),
                    ))));
          });
        },
      ),
      floatingActionButton: FloatingActionButton(
        onPressed: () => bloc.addAction.add(null),
        tooltip: "Add",
        child: const Icon(Icons.add),
      ),
    );
  }
}
