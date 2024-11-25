import { json2xml, js2xml, xml2json, xml2js } from "xml-js";

const rawxml = `<request type="FLIGHTAVAILABILITY">
<fromcity>CGK</fromcity>
<tocity>DPS</tocity>
<departdate>19-Jul-2019</departdate>
<listairline>
<airline>GA</airline>
<airline>JT</airline>
</listairline>
<cabin></cabin>
<adult>1</adult>
<child>0</child>
<infant>0</infant>
<cheapestclass>true</cheapestclass>
</request>`

const thejson = xml2json(rawxml, { compact: true, captureSpacesBetweenElements: true, spaces: 2 });
console.log("the json:", thejson);
const theobj: { [key: string]: any} = xml2js(rawxml, {compact: true})
console.log("the object:", theobj)

console.log("theobj.request:", theobj.request)

const j2xraw = {
    request: {
        _attributes: {
            "type": "FLIGHTAVAILABILITY",
        },
        fromcity: { _text: "CGK" },
        tocity: { _text: "DPS" },
        departdate: { _text: "19-Jul-2019" },
        listairlines: {
            airline: [
                { _text: "GA" },
                { _text: "JT" },
            ],
        },
        cabin: {},
        adult: { _text: 1 },
        child: { _text: 0 },
        infant: { _text: 0 },
        cheapestclass: { _text: true }
    },
}
const jraw = JSON.stringify(j2xraw)
const thexml = json2xml(jraw, { spaces: 2, compact: true, fullTagEmptyElement: true })
console.log("jraw:", jraw)
console.log("thexml:", thexml)
console.log(`are both same? ${rawxml.replace(/\s/g, "") === thexml.replace(/\s/g, "")}`)
const thexml2 = js2xml(j2xraw, { compact: true, fullTagEmptyElement: true })
console.log("thexml2:", thexml2)
console.log(`are both same? ${rawxml.replace(/\s/g, "") === thexml2.replace(/\s/g, "")}`)

// type Natural = number & { __nonNegative: true }
// type Natural2 = Exclude<number, number>

// interface MyNumNotNegative {
//     canNegative: number,
//     alwaysPositive: Natural,
//     positive2: Natural2,
// }

// const mypos = { canNegative: -5, alwaysPositive: -5, positive2: -5 } as MyNumNotNegative 
// console.log(mypos)

// type NonNegativeInteger<T extends number> =
//     number extends T
//     ? never
//     : `${T}` extends `-${string}` | `${string}.${string}`
//     ? never
//     : T;

// interface MyNumNotNegative2<N extends number> {
//     canNegative: number,
//     alwaysPositive: NonNegativeInteger<N>,
//     positive2: NonNegativeInteger<N>,
// }

// const mypos2 = { canNegative: -5, alwaysPositive: -5, positive2: -5 } as MyNumNotNegative2<number>
// console.log(mypos2)

interface Request {
    request: {
        _attributes: { [key: string]: any },
        fromcity: string,
        tocity: string,
        departdate: string,
        returndate?: string,
        listairlines: {
            airline: string[],
        },
        cabin?: number,
        adult: number
        child?: number,
        infant?: number,
        cheapestclass?: boolean,
    },
}

const vanj2xraw: Request = {
    request: {
        _attributes: {
            "type": "FLIGHTAVAILABILITY",
        },
        fromcity: "CGK",
        tocity: "DPS",
        departdate: "19-Jul-2019",
        listairlines: {
            airline: [ "GA", "JT" ],
        },
        adult: 1,
        cheapestclass: true,
    },
}

console.log(js2xml(vanj2xraw, { compact: true }))

interface Flight {
    flightno: string;
    airlinecode: string;
    fromcity: string;
    tocity: string;
    departdate: string;
    arrivaldate: string;
    departtime: string;
    arrivetime: string;
    durationHour: string;
    durationMinute: string;
    stopover: string;
    dayexchange: string;
    meal: string;
}

interface Class {
    code: string;
    availcode: string;
    fare: string;
    currency: string;
    pairid: string;
    baggagekilos: string;
    cabin: string;
    servicefee: string;
}

interface ClassGroup {
    class: Class;
}

interface ListItem {
    item: {
        durationHour: string;
        durationMinute: string;
        throughfare: string;
        dayexchange: string;
        listflight: {
            flight: Flight;
        };
        listclassgroup: {
            classgroup: ClassGroup;
        };
    };
}

interface Trip {
    type: string;
    departure: {
        listitem: ListItem;
    };
}

interface Response {
    type: string;
    trip: Trip;
}

interface XMLResponse {
    response: Response;
}

function walkObject(o: { [k: string]: any}, spaces: number) {
    for (const [k, v] of Object.entries(o)) {
        if (k === "_comment") continue
        console.log(`${"".repeat(spaces)}key ${k} with typeof ${typeof k}: value ${v} with typeof ${typeof v}`)
        if (v && typeof v === "object") {
            walkObject(v, spaces+2)
        }

    }
}

function member<T>(o:T, os: T[]): boolean {
    for (const oo of os) {
        // console.write(`o: ${o} and oo: ${oo}\t`)
        if (o === oo) return true
    }
    // console.log()
    return false
}

function hasAttributes(o: {[k: string]: any}): boolean {
    for (const k of Object.keys(o)) {
        if (k.startsWith("_") && k !== "_text") return true
    }
    return false
}

function convertText(v: { [k: string]: any }): any {
    let result: any
    result = v._text
    const num = Number.parseInt(result)
    if (!Number.isNaN(num)) {
        result = num
    } else if (typeof result === "string" &&
        member(result.toLowerCase(), ["true", "false"])) {
        result = result.toLowerCase() === "true"
    }
    return result

}

function convertObject(v: { [k: string]: any }): any {
    let result: any
    if (Object.keys(v).length === 1 && v._text) {
        return convertText(v)
    } else if (Object.keys(v).find(k => k.startsWith("_") && k !== "_text") && v._text) {
        result = convertType(v, false)
        result._value = convertText(v)
        return result
    }
    if (Object.values(v).find(vv => Array.isArray(v))) {
        result = convertType(v, true)
        return result
    }
    return convertType(v, false)

}

function fillAttributes(target: {[k: string]: any}, source: {[k: string]: any}) {
    for (const [k, v] of Object.entries(source)) {
        target["_"+k] = v
    }
}

/// convertType is converting the result from xml2js which populate the attributes
/// of element in field _attributes and spread its properties as parent properties
/// with prefixed by _ (underscore).
/// Exception with _text -> _value because some elements have both attributes and value
export function convertType(source: { [k: string]: any }, ignoreText = false): { [k: string]: any } {
    let result: {[k: string]: any} = {}
    for (const [k, v] of Object.entries(source)) {
        let kk = k.trim()
        if (kk === "_comment" ||
            (ignoreText && kk === "_text")) continue
        if (!v) continue
        if (Array.isArray(v)) {
            result[kk] = v.map((value: any) => convertType(value, false))
            continue
        }
        if (kk === "_attributes") {
            Object.entries(v).forEach(arr => result["_"+arr[0]] = arr[1])
            continue
        }
        if (typeof v === "object") {
            result[kk] = convertObject(v)
        }
    }
    return result
}

import path from "path";

(async () => {
    const fpath = "./van-flight-availability-response.xml"
    const fname = path.parse(fpath).name
    const fvanrespraw = Bun.file(fpath)
    const rawreqavail = await fvanrespraw.text()
    const thejs: { [key: string]: any } = xml2js(rawreqavail, { compact: true })
    console.log("thejs:", thejs)
    const thejson = xml2json(rawreqavail, { compact: true, spaces: 2 })
    console.log("thejson:", thejson)

    walkObject(thejs, 0)
    const myob = convertType(thejs, false)
    const orimyob = myob
    const jsonmyob = JSON.stringify(myob)
    console.log("myob:", jsonmyob)
    await Bun.write(fname + "2.json", JSON.stringify(myob, null, 2))
    for (const i in myob.response.trip) {
        const mytriptype = myob.response.trip[i].type
        myob.response.trip[i]._attributes = {
            type: mytriptype,
        }
        delete myob.response.trip[i].type
    }
    // console.log("myob.response.trip", myob.response.trip)
    const retmyob = reattributeType2(orimyob)
    await Bun.write(fname + "2.xml", json2xml(JSON.stringify(myob), { spaces: 2, compact: true }))
    reattributeType(orimyob)
    // console.log("orimyob.response.trip", orimyob.response.trip)
    await Bun.write(fname + "3.json", JSON.stringify(orimyob, null, 2))
    await Bun.write(fname + "3.xml", json2xml(JSON.stringify(orimyob), { spaces: 2, compact: true }))
    await Bun.write(fname + "4.json", JSON.stringify(retmyob, null, 2))
    await Bun.write(fname + "4.xml", json2xml(JSON.stringify(retmyob), { spaces: 2, compact: true }))

})()

function reattributeType(source: {[k: string]: any}) {
    for (let [k, o] of Object.entries(source)) {
        if (k === "type" && typeof o === "string") {
            console.log(`Got field type, k: ${k}, o: ${o}`)
            source._attributes = {
                type: o,
            }
            delete source.type
        } else if (Array.isArray(o)) {
            for (let e of o) {
                reattributeType(e)
            }
        } else if (o && typeof o === "object") {
            reattributeType(o)
        }

    }
}

function reattributeType2(source: {[k: string]: any}): {[k: string]: any} {
    let result: {[k: string]: any} = {}
    let attribs: {[k: string]: any} = {}
    for (let [k, o] of Object.entries(source)) {
        if (k === "_value") {
            result._text = o
        } else if (k.startsWith("_") && typeof o === "string") {
            attribs[k.slice(1)] = o
        } else if (Array.isArray(o)) {
            result[k] = o.map(reattributeType2)
        } else if (o && typeof o === "object") {
            result[k] = reattributeType2(o)
        } else {
            result[k] = source[k]
        }
    }
    if (Object.keys(attribs).length > 0) {
        result._attributes = attribs
    }
    return result
}

(async () => {
    const fpath = "./van-flight-fare-response.xml"
    const fname = path.parse(fpath).name
    const fvanrespraw = Bun.file(fpath)
    const rawreqavail = await fvanrespraw.text()
    const thejs: { [key: string]: any } = xml2js(rawreqavail, { compact: true })
    console.log("thejs:", thejs)
    const thejson = xml2json(rawreqavail, { compact: true, spaces: 2 })
    console.log("thejson:", thejson)
    await Bun.write(fname + ".json", thejson)

    walkObject(thejs, 0)
    const myob = convertType(thejs, false)
    const orimyob = myob
    const jsonmyob = JSON.stringify(myob)
    console.log("myob:", jsonmyob)
    await Bun.write(fname + "2.json", JSON.stringify(myob, null, 2))
    for (const i in myob.response.trip) {
        const mytriptype = myob.response.trip[i].type
        myob.response.trip[i]._attributes = {
            type: mytriptype,
        }
        delete myob.response.trip[i].type
    }
    // console.log("myob.response.trip", myob.response.trip)
    const retmyob = reattributeType2(orimyob)
    await Bun.write(fname + "2.xml", json2xml(JSON.stringify(myob), { spaces: 2, compact: true }))
    reattributeType(orimyob)
    // console.log("orimyob.response.trip", orimyob.response.trip)
    await Bun.write(fname + "3.json", JSON.stringify(orimyob, null, 2))
    await Bun.write(fname + "3.xml", json2xml(JSON.stringify(orimyob), { spaces: 2, compact: true }))
    await Bun.write(fname + "4.json", JSON.stringify(retmyob, null, 2))
    await Bun.write(fname + "4.xml", json2xml(JSON.stringify(retmyob), { spaces: 2, compact: true }))

})()