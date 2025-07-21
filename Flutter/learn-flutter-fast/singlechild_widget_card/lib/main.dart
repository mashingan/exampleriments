import 'package:flutter/material.dart';

void main() {
  runApp(const MainApp());
}

class MainApp extends StatelessWidget {
  const MainApp({super.key});

  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      home: NewsfeedWidget(),
    );
  }
}

class News {
  final DateTime _dt;
  final String _title, _text;
  News(this._dt, this._title, this._text);

  get dt => _dt;
  get title => _title;
  get text => _text;
}

class NewsfeedWidget extends StatelessWidget {
  NewsfeedWidget({super.key});

  final _newslist = [
    News(DateTime(2018, 12, 1), "Mass shooting in Atlanta",
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Proin sit amet tortor pretium, interdum magna sed, pulvinar ligula."),
    News(DateTime(2019, 1, 12), "Clown found drunk in Misisippi",
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Proin sit amet tortor pretium, interdum magna sed, pulvinar ligula."),
    News(DateTime(2018, 2, 12), "Walrus found in family pool in Florida",
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Proin sit amet tortor pretium, interdum magna sed, pulvinar ligula."),
  ];

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: const Text("News Feed"),
      ),
      body: Center(
          child: ListView(
              padding: const EdgeInsets.all(20),
              children: [for (var n in _newslist) NewsCard(n)])),
    );
  }
}

class NewsCard extends StatelessWidget {
  final News _news;
  const NewsCard(this._news, {super.key});
  @override
  Widget build(BuildContext context) {
    return Padding(
      padding: const EdgeInsets.only(bottom: 20),
      child: Card(
          child: Padding(
        padding: const EdgeInsets.all(20),
        child: Column(
          crossAxisAlignment: CrossAxisAlignment.start,
          children: [
            Image.network(
                "https://www.bbc.co.uk/news/special/2015/newsspec_10857/bbc_news_logo.png?cb=1"),
            Padding(
              padding: const EdgeInsets.only(top: 20, bottom: 10),
              child: Text(
                "${_news.dt.month}//${_news.dt.day}/${_news.dt.year}",
                style:
                    const TextStyle(fontSize: 10, fontStyle: FontStyle.italic),
              ),
            ),
            Padding(
              padding: const EdgeInsets.only(bottom: 10),
              child: Text(
                "${_news.title}",
                style:
                    const TextStyle(fontSize: 20, fontWeight: FontWeight.bold),
              ),
            ),
            Text(
              _news._text,
              maxLines: 2,
              style: const TextStyle(fontSize: 14),
              overflow: TextOverflow.fade,
            ),
            Row(
              children: [
                for (var s in ["Share", "Bookmark", "Link"])
                  TextButton(onPressed: () {}, child: Text(s))
              ],
            )
          ],
        ),
      )),
    );
  }
}
