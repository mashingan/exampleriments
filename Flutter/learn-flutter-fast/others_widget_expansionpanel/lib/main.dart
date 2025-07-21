import 'package:flutter/material.dart';
import 'package:get/get.dart';

void main() {
  runApp(GetMaterialApp(
    home: const MainApp(),
    title: "Flutter Demo",
    theme: ThemeData(useMaterial3: true, primarySwatch: Colors.green),
    // showPerformanceOverlay: true, // cannot support web and linux
  ));
}

class ExpansionPanelData extends GetxController {
  final String _title;
  final String _body;
  Rx<bool> expanded;

  ExpansionPanelData(String title, String body, bool expanded)
      : _title = title,
        _body = body,
        expanded = expanded.obs;

  @override
  String toString() {
    return "ExpansionPanelData{title: \"$_title\", body: \"$_body\", expanded: $expanded}";
  }

  String get body => _body;
  String get title => _title;
}

const _lorem =
    "Debitis et eos facere est molestiae. Enim architecto ut aperiam provident fugiat. "
    "Ad omnis ut magni velit enim. "
    "Facilis nemo tempora expedita sed autem dignissimos. "
    "Est aperiam esse officia. "
    "Aut voluptatem quaerat deserunt amet est. "
    "Et perferendis fuga a exercitationem consequatur enim aut. "
    "Reprehenderit quibusdam quo est minus qui ut. "
    "Tenetur quis placeat temporibus consequatur quis aut explicabo praesentium.";

class MainAppState extends GetxController {
  var data = <ExpansionPanelData>[].obs;

  @override
  void onInit() {
    super.onInit();
    var titles = [
      "Can I backup my data?",
      "How can I incresae my space?",
      "How do I cancel?",
      "How do I change language?",
      "How do I search?",
      "How do I view on other devices?",
      "How do I view my history?",
      "Is my subscription cost going to go up?"
    ];
    for (var title in titles) {
      data.add(ExpansionPanelData(title, _lorem, false));
    }
  }
}

class MainApp extends StatelessWidget {
  const MainApp({super.key});

  void _onExpansion(MainAppState mas, int panelIndex) {
    mas.data[panelIndex].expanded.toggle();
  }

  @override
  Widget build(BuildContext context) {
    final MainAppState mas = Get.put(MainAppState());
    return Scaffold(
      appBar: AppBar(title: const Text("FAQs")),
      body: SingleChildScrollView(
          child: Container(
        margin: const EdgeInsets.all(24),
        child: Obx(() => ExpansionPanelList(
              children: [
                for (var data in mas.data)
                  ExpansionPanel(
                      headerBuilder: (BuildContext context, bool isExpanded) {
                        return Padding(
                            padding: const EdgeInsets.all(20),
                            child: Text(
                              data.title,
                              style: const TextStyle(
                                  fontSize: 20, fontWeight: FontWeight.bold),
                            ));
                      },
                      body: Padding(
                        padding: const EdgeInsets.all(20),
                        child: Text(
                          data.body,
                          style: const TextStyle(
                              fontSize: 16, fontStyle: FontStyle.italic),
                        ),
                      ),
                      isExpanded: data.expanded.value)
              ],
              expansionCallback: (index, isExpanded) {
                _onExpansion(mas, index);
              },
            )),
      )),
    );
  }
}
