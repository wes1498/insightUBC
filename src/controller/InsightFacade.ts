import Log from "../Util";
import {
    IInsightFacade,
    InsightDataset,
    InsightDatasetKind,
    InsightError,
    InsightFilter,
    NotFoundError,
    IGeoResponse,
    InsightRoom,
} from "./IInsightFacade";
import * as JSZip from "jszip";
import {JSZipObject} from "jszip";
import * as fs from "fs";
import * as parse5 from "parse5/lib";
import * as http from "http";

/**
 * This is the main programmatic entry point for the project.
 * Method documentation is in IInsightFacade
 *
 */

export default class InsightFacade implements IInsightFacade {
    private coursesMap: Map<string, any>;
    private roomsMap: Map<string, any[]>;
    private linksMap: Map<string, any>;
    constructor() {
        Log.trace("InsightFacadeImpl::init()");
        this.coursesMap = new Map<string, any>();
        this.roomsMap = new Map<string, any[]>();
        this.linksMap = new Map<string, any>();
    }
// ADD ROOMS METHODS
    private getGeoInfo(url: string): Promise<IGeoResponse> {
        return new Promise<IGeoResponse>((resolve, reject) => {
            http.get(url, (res) => {
                res.setEncoding("utf8");
                let body = "";
                res.on("data", (stream) => {
                    body += stream;
                }).on("error", (err) => {
                    reject(err);
                });
                res.on("end", () => {
                    let result: IGeoResponse = JSON.parse(body);
                    resolve(result);
                });
            });
        });
    }
    private storeData(data2: any, data3: any,  id: string, code: any): Promise<any> {
        return new Promise<any>((resolve, reject) => {
            let arrnum: any[] = [];
            let arrtype: any[] = [];
            let arrfurn: any[] = [];
            let arrsize: any[] = [];
            let arrhref: any[] = [];
            let res: any[] = [];
            let res2: any[] = [];
            let resultsaver: any[] = [];
            data3[0].forEach((numElem: any) => {
                arrnum.push(numElem);
            });
            data3[1].forEach((typeElem: any) => {
                arrtype.push(typeElem);
            });
            data3[2].forEach((furnElem: any) => {
                arrfurn.push(furnElem);
            });
            data3[3].forEach((sizeElem: any) => {
                arrsize.push(sizeElem);
            });
            data3[4].forEach((sizeElem: any) => {
                arrhref.push(sizeElem);
            });
            data3[0].forEach((elem: any, i: any) => {
                let str1 = arrtype[i].replace(code + ": ", "type:");
                let str2 = arrfurn[i].replace(code + ": ", "furniture:");
                let str3 = arrsize[i].replace(code + ": ", "seats:");
                let str4 = arrhref[i].replace(code + ": ", "href:");
                res.push(elem + str1 + str2 + str3 + str4);
            });
            data2.forEach((item: any) => {
                res2.push(item);
                // console.log(item);
            });
            let addr = res2[1].replace(/ /g, "%20");
            let url = "http://cs310.ugrad.cs.ubc.ca:11316/api/v1/project_e6y0b_s5c1b/" + addr;

            this.getGeoInfo(url).then((data4: any) => {
                let latlon = JSON.parse(JSON.stringify(data4));
                let lat = latlon.lat;
                let lon = latlon.lon;
                res.forEach( (item: any) => {
                    let num = item.replace("number:", "");
                    let num2 = num.replace(/type:.*/, "");
                    let seat = item.replace(/.*seats:/, "");
                    let seat2 = seat.replace(/href:.*/, "");
                    let seat3 = Number(seat2);
                    let type = item.replace(/.*type:/, "");
                    let type2 = type.replace(/furniture.*/, "");
                    // Obtained regex online
                    let type3 = type2.replace(/([A-Z])/g, " $1").trim();
                    let furn = item.replace(/.*furniture:/, "");
                    let furn2 = furn.replace(/seats:.*/, "");
                    let href = item.replace(/.*href:/, "");
                    let roomsname = code + "_" + num2;
                    let obj = {
                        rooms_fullname: res2[0], rooms_shortname: code, rooms_number: num2,
                        rooms_name: roomsname, rooms_address: res2[1],
                        rooms_lat: lat, rooms_lon: lon, rooms_seats: seat3,
                        rooms_type: type3, rooms_furniture: furn2, rooms_href: href
                    };
                    let roomsobj = JSON.stringify(obj);
                    resultsaver.push(roomsobj);
                });
                return resolve(resultsaver);
            });
        });
    }
    // ADDS ROOM DATA FURNITURE ADDRESS FULL NAME ETC
    private roomData(content: any, id: string) {
        return new Promise<any>((resolve, reject) => {
            let resultsaver: any[] = [];
            let roomCodes = Array.from(this.linksMap.keys());
            JSZip.loadAsync(content, {base64: true}).then((zipRooms: JSZip) => {
                roomCodes.forEach((code: any) => {
                    let value = this.linksMap.get(code);
                    let subs = value[0].slice(2, 47);
                    zipRooms.file(subs).async("text").then((data: any) => {
                        Promise.all([this.getData(data, code),
                            this.getDataRoomNum(data, code)]).then((abc: any) => {
                            this.storeData(abc[0], abc[1], id, code).then((lon: any) => {
                                // console.log(resolve1);
                                // console.log(lon);
                                // resultsaver.push(lon);
                                // console.log(resultsaver);
                                // console.log(lon);
                                // console.log(lon);
                                this.roomsMap.get(id).push(lon);
                                console.log(lon);
                                // console.log("newwww iteration---------------------------------------------------");
                                // console.log(lon);
                                // this.roomsMap.get(id).push(lon);
                                // console.log(this.roomsMap);
                                // return resolve(this.roomsMap);
                            });
                            // return resolve(resultsaver);
                        });
                    });
                });
            });
        });
    }
    // GETS LINKS FROM INDEX.HTM IT ALSO GETS SHORT NAME
    private addRoom(data: any, code: string) {
        // let doc: any = parse5.parse(data);
        // let data2 = data.async("text");
        // console.log(data2);
        let document: any = parse5.parse(data);
        let dataNodes: any[] = document.childNodes;
        dataNodes.forEach((childnode: any) => {
            if (childnode.tagName === "html") {
                // this.addRoom2(childnode);
                this.getDataLinks(childnode);
            }
        });
        // console.log(this.roomsMap);
        // console.log(dataNodes);
    }
    private getDataLinks(data: any) {
        let dataNodes: any[] = data.childNodes;
        let classVal = "building-info";
        dataNodes.forEach((childnode: any) => {
            if (childnode.tagName === null || childnode.tagName === undefined) {
                // console.log("undef");
            } else if (childnode.nodeName !== "tbody") {
                this.getDataLinks(childnode);
            } else {
                this.getDataLinks2(childnode);
            }
        });
    }
    private getDataLinks2(data: any) {
        let dataNodes: any[] = data.childNodes;
        dataNodes.forEach((childnode: any) => {
            if (childnode.tagName === null || childnode.tagName === undefined) {
                // console.log("undef");
            } else if (childnode.nodeName !== "a") {
                this.getDataLinks2(childnode);
            } else {
                this.getLinks(childnode.attrs);
            }
        });
    }
    private getLinks(data: any) {
        let res: any[] = [];
        let regex = /.\/campus\/discover\/buildings-and-classrooms\/.*/;
        data.forEach((obj: any) => {
            if (obj.value.match(regex)) {
                res.push(obj.value);
            }
        });
        this.removeDuplicates(res);
    }
    private removeDuplicates(data: any[]) {
        // console.log(data);
        let sub = data[0].substring(43, 47);
        // console.log(sub);
        let elem = data[0];
        this.linksMap.set(sub, data);
        let merged = [].concat.apply([], data);
        // console.log(merged);
        // this.check();
    }
    // GET DATA FROM THE ROOMS FROM THE LINKS
    private getData(data: any, code: string): Promise<any> {
        return new Promise<any[]>((resolve, reject) => {
            let promises: any[] = [];
            let document: any = parse5.parse(data);
            let dataNodes: any[] = document.childNodes;
            dataNodes.forEach((childnode: any) => {
                if (childnode.tagName === "html") {
                    // this.getDatar(childnode, code);
                    // this.getDatar2(childnode, code);
                    // this.getDataMix(childnode, code);
                    this.getDataName(childnode, code).then((data2: any) => {
                        this.getDataAddress(childnode, code).then((data3: any) => {
                            return resolve([data2, data3]);
                        });
                        // console.log(data2);
                        // promises.push(data2);
                        // return resolve(data2);
                    });
                    // this.getDataAddress(childnode, code);
                    // this.vie();
                }
            });
        });
        // console.log(this.nameMap);
        // console.log(promises);
    }
    // GET DATA FROM ROOM NUMBER
    private getDataRoomNum(data: any, code: string): Promise<any> {
        return new Promise<any[]>((resolve, reject) => {
            let document: any = parse5.parse(data);
            let dataNodes: any[] = document.childNodes;
            dataNodes.forEach((childnode: any) => {
                if (childnode.tagName === "html") {
                    this.getRoomNumber(childnode, code).then((data2: any) => {
                        this.getRoomType(childnode, code).then((data3: any) => {
                            this.getRoomFurn(childnode, code).then((data4: any) => {
                                this.getRoomSize(childnode, code).then((data5: any) => {
                                    this.getRoomHref(childnode, code).then((data6: any) => {
                                        // console.log(data3);
                                        return resolve([data2, data3, data4, data5, data6]);
                                    });
                                });
                            });
                        });
                    });
                }
            });
        });
    }
    // GET DATA NAME
    private getDataName(data: any, code: string): Promise<any> {
        return new Promise<any[]>((resolve, reject) => {
            let dataNodes: any[] = data.childNodes;
            let classVal = "building-info";
            dataNodes.forEach((childnode: any) => {
                if (childnode.tagName === null || childnode.tagName === undefined) {
                    // console.log("undef");
                } else if (childnode.tagName !== "h2") {
                    this.getDataName(childnode, code).then((data2: any) => {
                        return resolve(data2);
                    });
                } else {
                    // console.log(this.getDataName2(childnode, code));
                    this.getDataName2(childnode, code).then((data2: any) => {
                        // console.log(data2);
                        return resolve(data2);
                    });
                }
            });
        });
    }
    // GET DATA NAME
    private getDataName2(data: any, code: string): any {
        return new Promise<any[]>((resolve, reject) => {
            let dataNodes: any[] = data.childNodes;
            dataNodes.forEach((childnode: any) => {
                if (childnode.tagName === "span") {
                    // console.log(childnode);
                    this.getDataName3(childnode, code).then((data2: any) => {
                        // console.log(data2);
                        return resolve(data2);
                    });
                }
            });
        });
    }
    // GET DATA NAME
    private getDataName3(data: any, code: string): any {
        return new Promise<any[]>((resolve, reject) => {
            let dataNodes: any[] = data.childNodes;
            dataNodes.forEach((childnode: any) => {
                if (childnode.nodeName === "#text") {
                    // this.nameMap.set(code, childnode.value);
                    // console.log(code + ": " + childnode.value);
                    return resolve(childnode.value);
                }
            });
        });
    }
    // GET DATA ADDRESS
    private getDataAddress(data: any, code: string) {
        return new Promise<any[]>((resolve, reject) => {
            let dataNodes: any[] = data.childNodes;
            let classVal = "building-info";
            dataNodes.forEach((childnode: any) => {
                if (childnode.tagName === null || childnode.tagName === undefined) {
                    // console.log("undef");
                } else if (childnode.tagName !== "div") {
                    this.getDataAddress(childnode, code).then((data2: any) => {
                        // console.log(data2);
                        return resolve(data2);
                    });
                } else {
                    if (typeof childnode.attrs !== "undefined") {
                        childnode.attrs.forEach((at: any) => {
                            if (childnode.attrs.length >= 1) {
                                if (at.value === classVal) {
                                    // console.log(childnode);
                                    this.getDataAddress5(childnode, code).then((data2: any) => {
                                        return resolve(data2);
                                    });
                                } else {
                                    this.getDataAddress(childnode, code).then((data2: any) => {
                                        return resolve(data2);
                                    });
                                }
                            }
                        });
                    }
                }
            });
        });
    }
    // GET DATA ADDRESS
    private getDataAddress5(data: any, code: string) {
        return new Promise<any[]>((resolve, reject) => {
            let dataNodes: any[] = data.childNodes;
            dataNodes.forEach((childnode: any) => {
                if (childnode.tagName === "div") {
                    this.getDataAddress6(childnode, code).then((data2: any) => {
                        return resolve(data2);
                    });
                }
            });
        });
    }
    // GET DATA ADDRESS
    private getDataAddress6(data: any, code: string) {
        return new Promise<any[]>((resolve, reject) => {
            let dataNodes: any[] = data.childNodes;
            dataNodes.forEach((childnode: any) => {
                if (childnode.tagName === "div") {
                    // console.log(childnode);
                    this.getDataAddress7(childnode, code).then((data2: any) => {
                        return resolve(data2);
                    });
                }
            });
        });
    }
    // GET DATA ADDRESS
    private getDataAddress7(data: any, code: string) {
        return new Promise<any[]>((resolve, reject) => {
            let dataNodes: any[] = data.childNodes;
            dataNodes.forEach((childnode: any) => {
                if (childnode.nodeName === "#text") {
                    if (childnode.value.substring(0, 8) === "Building") {
                        // nda
                    } else {
                        return resolve(childnode.value);
                    }
                }
            });
        });
    }
    private getRoomNumber(data: any, code: string): Promise<any[]> {
        return new Promise<any[]>((resolve, reject) => {
            let dataNodes: any[] = data.childNodes;
            let c1 = "odd";
            let c2 = "even";
            let c3 = "even views-row-last";
            let c4 = "odd views-row-last";
            let c5 = "even views-row-first";
            let c6 = "odd views-row-first";
            let promises: any[] = [];
            let res: any[] = [];
            dataNodes.forEach((childnode: any) => {
                if (childnode.tagName === null || childnode.tagName === undefined || childnode.length === 0) {
                    // console.log("undef");
                } else if (childnode.tagName !== "tr") {
                    // Promise.all()
                    this.getRoomNumber(childnode, code).then((data2: any) => {
                        // Promise.all(promises).then((data3) => {
                        // this.numArray.push(code + ": " + data2);
                        // console.log(code + ": " + data2);
                        // promises.push(data2);
                        // });
                        return resolve(data2);
                    });
                } else {
                    promises.push(childnode);
                    // console.log(childnode);
                }
            });
            if (promises.length !== 0) {
                if (promises === undefined) {
                    // c
                } else {
                    this.getRoomNumber2(promises, code).then((data2: any) => {
                        res = data2;
                        return resolve(data2);
                    });
                }
            }
            /*Promise.all(promises).then((data3) => {
                 console.log(promises);
                 return resolve(data3);
            });*/
        });
    }
    private getRoomNumber2(prom: any, code: string): any {
        return new Promise<any[]>((resolve, reject) => {
            //  let dataNodes: any[] = data.childNodes;
            let c1 = "odd";
            let c2 = "even";
            let c3 = "even views-row-last";
            let c4 = "odd views-row-last";
            let c5 = "even views-row-first";
            let c6 = "odd views-row-first";
            let promises: any[] = [];
            prom.forEach((datatr: any) => {
                if (typeof datatr.attrs !== "undefined") {
                    datatr.attrs.forEach((at: any) => {
                        if (datatr.attrs.length >= 1) {
                            let x = at.value;
                            // console.log(x);
                            if (x === c1 || x === c2 || x === c3 || x === c4 || x === c5 || x === c6 ) {
                                // console.log(childnode);
                                // console.log(x);
                                promises.push(datatr);
                            }
                        }
                    });
                }
            });
            if (promises.length !== 0) {
                if (promises === undefined) {
                    // c
                } else {
                    // console.log(promises);
                    this.getRoomNumber3(promises, code).then((data2: any) => {
                        return resolve(data2);
                    });
                }
            }
        });
    }
    private getRoomNumber3(prom: any, code: string): any {
        return new Promise<any[]>((resolve, reject) => {
            // let dataNodes: any[] = data.childNodes;
            let classVal = "views-field views-field-field-room-number";
            let classVal2 = "views-field views-field-field-room-type";
            let promises: any[] = [];
            prom.forEach((datar: any) => {
                let x = datar.childNodes;
                //  console.log(x);
                x.forEach((dats: any) => {
                    if (dats.tagName === "td") {
                        // console.log("made it");
                        if (typeof dats.attrs !== "undefined") {
                            dats.attrs.forEach((atr: any) => {
                                // console.log(atr);
                                if (atr.value === classVal) {
                                    // console.log("eyyyyyyyyyy");
                                    // console.log(x);
                                    promises.push(dats);
                                }
                            });
                        }
                    }
                });
            });
            if (promises.length !== 0) {
                if (promises === undefined) {
                    // c
                } else {
                    // console.log(promises);
                    this.getRoomNumber4(promises, code).then((data2: any) => {
                        return resolve(data2);
                    });
                }
            }
        });
    }
    private getRoomNumber4(prom: any, code: string): any {
        return new Promise<any[]>((resolve, reject) => {
            let classVal = "views-field views-field-field-room-number";
            let classVal2 = "views-field views-field-field-room-type";
            let promises: any[] = [];
            // let dataNodes: any[] = data.childNodes;
            // this.numArray.push(code);
            // foreach prom, data attrs
            // prom.forEach((data4: any) => {
            // console.log(data4);
            // console.log("new arrayyyyyyyyyyyyyyyyyyy");
            // console.log(prom);
            prom.forEach((data2: any) => {
                let x = data2.childNodes;
                x.forEach((cnodes: any) => {
                    if (cnodes.tagName === "a") {
                        let y = cnodes.childNodes;
                        y.forEach((tex: any) => {
                            if (tex.nodeName === "#text") {
                                // console.log("newwwwwwwwwwww val");
                                // console.log(tex.value);
                                promises.push("number:" + tex.value);
                            }
                        });
                    }
                });
            });
            if (promises.length !== 0) {
                if (promises === undefined) {
                    // c
                } else {
                    return resolve(promises);
                }
            }
        });
    }
    private getRoomType(data: any, code: string): Promise<any[]> {
        return new Promise<any[]>((resolve, reject) => {
            let dataNodes: any[] = data.childNodes;
            let c1 = "odd";
            let c2 = "even";
            let c3 = "even views-row-last";
            let c4 = "odd views-row-last";
            let c5 = "even views-row-first";
            let c6 = "odd views-row-first";
            let promises: any[] = [];
            let res: any[] = [];
            dataNodes.forEach((childnode: any) => {
                if (childnode.tagName === null || childnode.tagName === undefined || childnode.length === 0) {
                    // console.log("undef");
                } else if (childnode.tagName !== "tr") {
                    // Promise.all()
                    this.getRoomType(childnode, code).then((data2: any) => {
                        return resolve(data2);
                    });
                } else {
                    promises.push(childnode);
                }
            });
            if (promises.length !== 0) {
                if (promises === undefined) {
                    // c
                } else {
                    // console.log(promises);
                    this.getRoomType2(promises, code).then((data2: any) => {
                        res = data2;
                        return resolve(data2);
                    });
                }
            }
        });
    }
    private getRoomType2(prom: any, code: string): any {
        return new Promise<any[]>((resolve, reject) => {
            //  let dataNodes: any[] = data.childNodes;
            let c1 = "odd";
            let c2 = "even";
            let c3 = "even views-row-last";
            let c4 = "odd views-row-last";
            let c5 = "even views-row-first";
            let c6 = "odd views-row-first";
            let promises: any[] = [];
            prom.forEach((datatr: any) => {
                if (typeof datatr.attrs !== "undefined") {
                    datatr.attrs.forEach((at: any) => {
                        if (datatr.attrs.length >= 1) {
                            let x = at.value;
                            // console.log(x);
                            if (x === c1 || x === c2 || x === c3 || x === c4 || x === c5 || x === c6 ) {
                                // console.log(childnode);
                                // console.log(x);
                                promises.push(datatr);
                            }
                        }
                    });
                }
            });
            if (promises.length !== 0) {
                if (promises === undefined) {
                    // c
                } else {
                    // console.log(promises);
                    this.getRoomType3(promises, code).then((data2: any) => {
                        return resolve(data2);
                    });
                }
            }
        });
    }
    private getRoomType3(prom: any, code: string): any {
        return new Promise<any[]>((resolve, reject) => {
            // let dataNodes: any[] = data.childNodes;
            // let classVal = "views-field views-field-field-room-number";
            let classVal2 = "views-field views-field-field-room-type";
            let promises: any[] = [];
            prom.forEach((datar: any) => {
                let x = datar.childNodes;
                //  console.log(x);
                x.forEach((dats: any) => {
                    if (dats.tagName === "td") {
                        // console.log("made it");
                        if (typeof dats.attrs !== "undefined") {
                            dats.attrs.forEach((atr: any) => {
                                // console.log(atr);
                                if (atr.value === classVal2) {
                                    // console.log("eyyyyyyyyyy");
                                    // console.log(x);
                                    promises.push(dats);
                                }
                            });
                        }
                    }
                });
            });
            if (promises.length !== 0) {
                if (promises === undefined) {
                    // c
                } else {
                    // console.log(promises);
                    this.getRoomType4(promises, code).then((data2: any) => {
                        return resolve(data2);
                    });
                }
            }
        });
    }
    private getRoomType4(prom: any, code: string): any {
        return new Promise<any[]>((resolve, reject) => {
            let promises: any[] = [];
            prom.forEach((data2: any) => {
                let x = data2.childNodes;
                x.forEach((cnodes: any) => {
                    let str = cnodes.value.replace(/\s/g, "");
                    promises.push(code + ": " + str);
                });
            });
            if (promises.length !== 0) {
                if (promises === undefined) {
                    // c
                } else {
                    // console.log(promises);
                    return resolve(promises);
                }
            }
        });
    }

    private getRoomFurn(data: any, code: string): Promise<any[]> {
        return new Promise<any[]>((resolve, reject) => {
            let dataNodes: any[] = data.childNodes;
            let c1 = "odd";
            let c2 = "even";
            let c3 = "even views-row-last";
            let c4 = "odd views-row-last";
            let c5 = "even views-row-first";
            let c6 = "odd views-row-first";
            let promises: any[] = [];
            let res: any[] = [];
            dataNodes.forEach((childnode: any) => {
                if (childnode.tagName === null || childnode.tagName === undefined || childnode.length === 0) {
                    // console.log("undef");
                } else if (childnode.tagName !== "tr") {
                    // Promise.all()
                    this.getRoomFurn(childnode, code).then((data2: any) => {
                        return resolve(data2);
                    });
                } else {
                    promises.push(childnode);
                }
            });
            if (promises.length !== 0) {
                if (promises === undefined) {
                    // c
                } else {
                    // console.log(promises);
                    this.getRoomFurn2(promises, code).then((data2: any) => {
                        res = data2;
                        return resolve(data2);
                    });
                }
            }
        });
    }
    private getRoomFurn2(prom: any, code: string): any {
        return new Promise<any[]>((resolve, reject) => {
            //  let dataNodes: any[] = data.childNodes;
            let c1 = "odd";
            let c2 = "even";
            let c3 = "even views-row-last";
            let c4 = "odd views-row-last";
            let c5 = "even views-row-first";
            let c6 = "odd views-row-first";
            let promises: any[] = [];
            prom.forEach((datatr: any) => {
                if (typeof datatr.attrs !== "undefined") {
                    datatr.attrs.forEach((at: any) => {
                        if (datatr.attrs.length >= 1) {
                            let x = at.value;
                            // console.log(x);
                            if (x === c1 || x === c2 || x === c3 || x === c4 || x === c5 || x === c6 ) {
                                // console.log(childnode);
                                // console.log(x);
                                promises.push(datatr);
                            }
                        }
                    });
                }
            });
            if (promises.length !== 0) {
                if (promises === undefined) {
                    // c
                } else {
                    // console.log(promises);
                    this.getRoomFurn3(promises, code).then((data2: any) => {
                        return resolve(data2);
                    });
                }
            }
        });
    }
    private getRoomFurn3(prom: any, code: string): any {
        return new Promise<any[]>((resolve, reject) => {
            // let dataNodes: any[] = data.childNodes;
            // let classVal = "views-field views-field-field-room-number";
            let classVal2 = "views-field views-field-field-room-furniture";
            let promises: any[] = [];
            prom.forEach((datar: any) => {
                let x = datar.childNodes;
                //  console.log(x);
                x.forEach((dats: any) => {
                    if (dats.tagName === "td") {
                        // console.log("made it");
                        if (typeof dats.attrs !== "undefined") {
                            dats.attrs.forEach((atr: any) => {
                                // console.log(atr);
                                if (atr.value === classVal2) {
                                    // console.log("eyyyyyyyyyy");
                                    // console.log(x);
                                    promises.push(dats);
                                }
                            });
                        }
                    }
                });
            });
            if (promises.length !== 0) {
                if (promises === undefined) {
                    // c
                } else {
                    // console.log(promises);
                    this.getRoomFurn4(promises, code).then((data2: any) => {
                        return resolve(data2);
                    });
                }
            }
        });
    }
    private getRoomFurn4(prom: any, code: string): any {
        return new Promise<any[]>((resolve, reject) => {
            let promises: any[] = [];
            prom.forEach((data2: any) => {
                let x = data2.childNodes;
                x.forEach((cnodes: any) => {
                    let str = cnodes.value.replace(/\s/g, "");
                    promises.push(code + ": " + str);
                });
            });
            if (promises.length !== 0) {
                if (promises === undefined) {
                    // c
                } else {
                    // console.log(promises);
                    return resolve(promises);
                }
            }
        });
    }
    private getRoomSize(data: any, code: string): Promise<any[]> {
        return new Promise<any[]>((resolve, reject) => {
            let dataNodes: any[] = data.childNodes;
            let c1 = "odd";
            let c2 = "even";
            let c3 = "even views-row-last";
            let c4 = "odd views-row-last";
            let c5 = "even views-row-first";
            let c6 = "odd views-row-first";
            let promises: any[] = [];
            let res: any[] = [];
            dataNodes.forEach((childnode: any) => {
                if (childnode.tagName === null || childnode.tagName === undefined || childnode.length === 0) {
                    // console.log("undef");
                } else if (childnode.tagName !== "tr") {
                    // Promise.all()
                    this.getRoomSize(childnode, code).then((data2: any) => {
                        return resolve(data2);
                    });
                } else {
                    promises.push(childnode);
                }
            });
            if (promises.length !== 0) {
                if (promises === undefined) {
                    // c
                } else {
                    // console.log(promises);
                    this.getRoomSize2(promises, code).then((data2: any) => {
                        res = data2;
                        return resolve(data2);
                    });
                }
            }
        });
    }
    private getRoomSize2(prom: any, code: string): any {
        return new Promise<any[]>((resolve, reject) => {
            //  let dataNodes: any[] = data.childNodes;
            let c1 = "odd";
            let c2 = "even";
            let c3 = "even views-row-last";
            let c4 = "odd views-row-last";
            let c5 = "even views-row-first";
            let c6 = "odd views-row-first";
            let promises: any[] = [];
            prom.forEach((datatr: any) => {
                if (typeof datatr.attrs !== "undefined") {
                    datatr.attrs.forEach((at: any) => {
                        if (datatr.attrs.length >= 1) {
                            let x = at.value;
                            // console.log(x);
                            if (x === c1 || x === c2 || x === c3 || x === c4 || x === c5 || x === c6 ) {
                                // console.log(childnode);
                                // console.log(x);
                                promises.push(datatr);
                            }
                        }
                    });
                }
            });
            if (promises.length !== 0) {
                if (promises === undefined) {
                    // c
                } else {
                    // console.log(promises);
                    this.getRoomSize3(promises, code).then((data2: any) => {
                        return resolve(data2);
                    });
                }
            }
        });
    }
    private getRoomSize3(prom: any, code: string): any {
        return new Promise<any[]>((resolve, reject) => {
            // let dataNodes: any[] = data.childNodes;
            // let classVal = "views-field views-field-field-room-number";
            let classVal2 = "views-field views-field-field-room-capacity";
            let promises: any[] = [];
            prom.forEach((datar: any) => {
                let x = datar.childNodes;
                //  console.log(x);
                x.forEach((dats: any) => {
                    if (dats.tagName === "td") {
                        // console.log("made it");
                        if (typeof dats.attrs !== "undefined") {
                            dats.attrs.forEach((atr: any) => {
                                // console.log(atr);
                                if (atr.value === classVal2) {
                                    // console.log("eyyyyyyyyyy");
                                    // console.log(x);
                                    promises.push(dats);
                                }
                            });
                        }
                    }
                });
            });
            if (promises.length !== 0) {
                if (promises === undefined) {
                    // c
                } else {
                    // console.log(promises);
                    this.getRoomSize4(promises, code).then((data2: any) => {
                        return resolve(data2);
                    });
                }
            }
        });
    }
    private getRoomSize4(prom: any, code: string): any {
        return new Promise<any[]>((resolve, reject) => {
            let promises: any[] = [];
            prom.forEach((data2: any) => {
                let x = data2.childNodes;
                x.forEach((cnodes: any) => {
                    let str = cnodes.value.replace(/\s/g, "");
                    promises.push(code + ": " + str);
                });
            });
            if (promises.length !== 0) {
                if (promises === undefined) {
                    // c
                } else {
                    // console.log(promises);
                    return resolve(promises);
                }
            }
        });
    }
    private getRoomHref(data: any, code: string): Promise<any[]> {
        return new Promise<any[]>((resolve, reject) => {
            let dataNodes: any[] = data.childNodes;
            let c1 = "odd";
            let c2 = "even";
            let c3 = "even views-row-last";
            let c4 = "odd views-row-last";
            let c5 = "even views-row-first";
            let c6 = "odd views-row-first";
            let promises: any[] = [];
            let res: any[] = [];
            dataNodes.forEach((childnode: any) => {
                if (childnode.tagName === null || childnode.tagName === undefined || childnode.length === 0) {
                    // console.log("undef");
                } else if (childnode.tagName !== "tr") {
                    // Promise.all()
                    this.getRoomHref(childnode, code).then((data2: any) => {
                        return resolve(data2);
                    });
                } else {
                    promises.push(childnode);
                }
            });
            if (promises.length !== 0) {
                if (promises === undefined) {
                    // c
                } else {
                    // console.log(promises);
                    this.getRoomHref2(promises, code).then((data2: any) => {
                        res = data2;
                        return resolve(data2);
                    });
                }
            }
        });
    }
    private getRoomHref2(prom: any, code: string): any {
        return new Promise<any[]>((resolve, reject) => {
            //  let dataNodes: any[] = data.childNodes;
            let c1 = "odd";
            let c2 = "even";
            let c3 = "even views-row-last";
            let c4 = "odd views-row-last";
            let c5 = "even views-row-first";
            let c6 = "odd views-row-first";
            let promises: any[] = [];
            prom.forEach((datatr: any) => {
                if (typeof datatr.attrs !== "undefined") {
                    datatr.attrs.forEach((at: any) => {
                        if (datatr.attrs.length >= 1) {
                            let x = at.value;
                            // console.log(x);
                            if (x === c1 || x === c2 || x === c3 || x === c4 || x === c5 || x === c6 ) {
                                // console.log(childnode);
                                // console.log(x);
                                promises.push(datatr);
                            }
                        }
                    });
                }
            });
            if (promises.length !== 0) {
                if (promises === undefined) {
                    // c
                } else {
                    // console.log(promises);
                    this.getRoomHref3(promises, code).then((data2: any) => {
                        return resolve(data2);
                    });
                }
            }
        });
    }
    private getRoomHref3(prom: any, code: string): any {
        return new Promise<any[]>((resolve, reject) => {
            // let dataNodes: any[] = data.childNodes;
            let classVal = "views-field views-field-field-room-number";
            let classVal2 = "views-field views-field-field-room-capacity";
            let promises: any[] = [];
            prom.forEach((datar: any) => {
                let x = datar.childNodes;
                //  console.log(x);
                x.forEach((dats: any) => {
                    if (dats.tagName === "td") {
                        // console.log("made it");
                        if (typeof dats.attrs !== "undefined") {
                            dats.attrs.forEach((atr: any) => {
                                // console.log(atr);
                                if (atr.value === classVal) {
                                    // console.log("eyyyyyyyyyy");
                                    // console.log(x);
                                    promises.push(dats);
                                }
                            });
                        }
                    }
                });
            });
            if (promises.length !== 0) {
                if (promises === undefined) {
                    // c
                } else {
                    // console.log(promises);
                    this.getRoomHref4(promises, code).then((data2: any) => {
                        return resolve(data2);
                    });
                }
            }
        });
    }
    private getRoomHref4(prom: any, code: string): any {
        return new Promise<any[]>((resolve, reject) => {
            let classVal = "views-field views-field-field-room-number";
            let classVal2 = "views-field views-field-field-room-type";
            let promises: any[] = [];
            prom.forEach((data2: any) => {
                let x = data2.childNodes;
                x.forEach((cnodes: any) => {
                    if (cnodes.tagName === "a") {
                        let y = cnodes.attrs;
                        y.forEach((tex: any) => {
                            if (tex.name === "href") {
                                // console.log("newwwwwwwwwwww val");
                                // console.log(tex.value);
                                promises.push(code + ": " + tex.value);
                            }
                        });
                    }
                });
            });
            if (promises.length !== 0) {
                if (promises === undefined) {
                    // c
                } else {
                    return resolve(promises);
                }
            }
        });
    }
    public addDataset(id: string, content: string, kind: InsightDatasetKind): Promise<string[]> {
        let that = this;
        return new Promise<string[]>(function (resolve, reject) {
            if (typeof id !== "string" || typeof content !== "string" || kind === undefined || kind === null) {
                reject(new InsightError("Invalid params"));
            }
            if (that.coursesMap.has(id)) {
                return reject(new InsightError("Id is already added"));
            }
            if (!id || id.length === 0 || id === "") {
                return reject(new InsightError("Invalid Id"));
            }
            that.coursesMap.set(id, []);
            JSZip.loadAsync(content, {base64: true}).then((unzipped: JSZip) => {

                let filesPromise: any[] = [];
                if (unzipped.files.hasOwnProperty("courses/") || unzipped.length > 0) {
                    unzipped.forEach((function (relativePath, fileObject: JSZipObject) {

                        filesPromise.push(fileObject.async("text").then((data: string) => {
                            try {
                                let parsedInfo = JSON.parse(data);
                                if ((parsedInfo.result.length === 0)) {
                                    throw new Error("None to store");
                                }
                                parsedInfo.result.forEach(function (section: any) {
                                    let dept: string = section.Subject;
                                    let iid: string = section.Course;
                                    let avg: number = section.Avg;
                                    let instructor: string = section.Professor;
                                    let title: string = section.Title;
                                    let pass: number = section.Pass;
                                    let fail: number = section.Fail;
                                    let audit: number = section.Audit;
                                    let uuid: string = section.id.toString();
                                    let year: number = Number(section.Year);

                                    // let validCourse: CourseSaver = new CourseSaver(dept, id,
                                    //     avg, instructor, title, pass, fail, audit, uuid, year);
                                    let validCourse: any = {
                                        dept: String,
                                        id: String,
                                        avg: Number,
                                        instructor: String,
                                        title: String,
                                        pass: Number,
                                        fail: Number,
                                        audit: Number,
                                        uuid: String,
                                        year: Number
                                    };
                                    validCourse.dept = dept;
                                    validCourse.id = iid;
                                    validCourse.avg = avg;
                                    validCourse.instructor = instructor;
                                    validCourse.title = title;
                                    validCourse.pass = pass;
                                    validCourse.fail = fail;
                                    validCourse.audit = audit;
                                    validCourse.uuid = uuid;
                                    validCourse.year = year;
                                    that.coursesMap.get(id).push(validCourse);
                                });

                            } catch (e) {
                                // not in JSON format or some fields of different type/missing-> skip this course
                            }
                        }).catch((e) => {
                            return reject(new InsightError("Error in adding dataset " + e));
                        }));
                    }));
                } else if (kind === InsightDatasetKind.Rooms) {
                        JSZip.loadAsync(content, {base64: true}).then((zipRooms: JSZip) => {
                            // console.log(zipRooms.file("index.htm"));
                            // console.log(zipRooms);
                            // console.log(zipRooms.file("index.htm").async("text"));
                            filesPromise.push(zipRooms.file("index.htm").async("text").then( (data: any) => {
                                // this.roomsMap.set(id, []);
                                this.addRoom(data, id);
                                this.roomData(content, id).then((x: any) => {
                                    // console.log(this.roomsMap);
                                });
                                // console.log(savearr);
                                // console.log(this.roomsMap);
                            }));
                        });
                        Promise.all(filesPromise).then((result) => {
                            // co
                        });
                } else {
                    return reject(new InsightError("Desired folder for the dataset kind does not exist"));
                }
                Promise.all(filesPromise).then(function () {
                    let toSaveOnDisk: object[] = that.coursesMap.get(id);
                    // if (toSaveOnDisk.length === 0) {
                    //     that.coursesMap.delete(id);
                    //     return reject(new InsightError("No sections were added"));
                    // } else {
                    fs.writeFile("data/" + id + ".json", JSON.stringify(toSaveOnDisk), function (e) {
                            return reject(new InsightError("Error Saving Files " + e));
                        });

                    let result: string[] = [];
                    that.coursesMap.forEach(function (value, key) {
                            result.push(key);
                        });
                    return resolve(result);
                    // }
                });
            }).catch((e) => {
                return reject(new InsightError("Error decoding contents: Invalid Zip " + e));
            });
        });
    }

    public removeDataset(id: string): Promise<string> {
        let that = this;
        return new Promise<string>(function (resolve, reject) {
            if (id === "") {
                return reject(new NotFoundError("Invalid ID"));
            } else if (id === null || !id) {
                return reject(new InsightError("Invalid ID"));
            } else if (!that.coursesMap.has(id)) {
                return reject(new NotFoundError("Id not in Map"));
            }
            if (that.coursesMap.has(id)) {
                fs.readdir(InsightDatasetKind.Courses.toString(), function (e, files) {
                    if (e) {
                        return reject(new InsightError("Dataset not added yet " + e));
                    }
                });
            }

            that.coursesMap.delete(id);
            return resolve (id);
        });
    }
    public performQuery(query: any): Promise<any[]> {
        let that = this;
        return new Promise<any[]>(function (resolve, reject) {
            try {
                let filter: InsightFilter = query.WHERE;
                let options = query.OPTIONS;
                let order = options.ORDER;
                let columns = options.COLUMNS;
                let id: string = columns[0].split("_")[0];
                // console.log(columns[0]);
                // console.log(columns);
                if (that.coursesMap.get(id) === undefined) {
                        reject(new InsightError("noot"));
                }
                // let sections = fs.readFileSync("./data/" + id + ".json", "UTF8");
                // if (!sections) {
                //     reject(new InsightError("id does not exist"));
                // }
                // let parsedInfo = JSON.parse(sections);
                let result: any[];
                if (Object.keys(filter).length === 0) {
                    result = that.coursesMap.get(id);
                    if (result.length > 5000) {
                        throw new InsightError("Too many sections in result");
                    }
                } else {
                    let thisResult: any[] = [];

                    for (let section of that.coursesMap.get(id)) {
                        // check if you can apply filter to the key
                        if (InsightFacade.isSectionValid(filter, section, id)) {
                            thisResult.push(section);
                        }
                    }
                    // result over 5000 to add
                    if (thisResult.length > 5000) {
                        throw new InsightError("Result exceeds 5000 limit");
                    }
                    result = thisResult;
                }
                // keep only the desired columns in query
                if (columns && columns.length !== 0) {
                    let columnResult: object[] = [];
                    result.forEach( function (section: any) {
                        let columnSection: any = {};
                        columns.forEach( function (key: any) {
                            if (InsightFacade.validKeyHelper(key, id)) {
                                let res = key.substring(0, key.indexOf("_"));
                                key = key.substring(key.indexOf("_") + 1);
                                columnSection[res + "_" + key] = section[key];
                            }
                        });
                        columnResult.push(columnSection);
                    });
                    result = columnResult;
                    // result = this.desiredColumnsHelper(result, columns);
                }
                // Sort the result if order is included
                if (order !== undefined || order !== null) {
                    if (columns.includes(order)) {
                        result = result.sort( function (a, b) {
                            let x = a[order];
                            let y = b[order];
                            if (x === y) {
                                return 0;
                            } else if (x > y ) {
                                return 1;
                            } else {
                                return -1;
                            }
                        });
                    } else {
                        throw new InsightError("ORDER not in COLUMNS");
                    }
                }
                // resolve if no problems
                return resolve(result);
            } catch (e) {
                return reject(new InsightError("Error in reading query"));
            }

        });
    }

    private static validKeyHelper(key: string, id: string): boolean {
        // check if the key being passed is a valid one
        switch (key) {
            case id + "_dept":
                return true;
            case id + "_id":
                return true;
            case id + "_instructor":
                return true;
            case id + "_title":
                return true;
            case id + "_uuid":
                return true;
            case id + "_avg":
                return true;
            case id + "_pass":
                return true;
            case id + "_fail":
                return true;
            case id + "_audit":
                return true;
            case id + "_year":
                return true;
            default:
                return false;
        }
    }

    // Check if filter applies to given section
    private static isSectionValid(filter: InsightFilter, section: any, id: string): boolean {
        if (filter.hasOwnProperty("AND")) {
            if (filter.AND.length < 1 ) {
                throw new InsightError("Not enough conditions for AND");
            }
                // for each filter section must be valid
                // recursive call to each filter in the array on the section
            for (let insideFilter of filter.AND) {
                if (!this.isSectionValid(insideFilter, section, id)) {
                    return false;
                }
            }
                // if all filters valid on each section
            return true;

        }
        if (filter.hasOwnProperty("OR")) {
            // OR must be 1 or more
            if (filter.OR.length === 0) {
                throw new InsightError("Not enough conditions for OR");
            }
                // for at least one filter section must be valid:
                // recursive call to each filter in the array on the section
            for (let insideFilter of filter.OR) {
                if (this.isSectionValid(insideFilter, section, id)) {
                    return true;
                }
            }
            return false;
        }
            // check if it is an MCOMPARATOR
        if (filter.hasOwnProperty("GT") || filter.hasOwnProperty("LT") || filter.hasOwnProperty("EQ")) {
            // this.validateMComFilterHelper(filter);
            let body = Object.values(filter)[0];
            // MCOMPARATOR must be a number
            // check for valid value
            if (typeof Object.values(body)[0] !== "number") {
                throw new InsightError("Invalid value");
            }
            // check for valid key
            if (!this.validKeyHelper(Object.keys(body)[0], id)) {
                throw new InsightError("Invalid key");
            }

            if (filter.hasOwnProperty("GT")) {
                let yup = Object.keys(filter.GT)[0]; // courses_avg
                yup = yup.substring(yup.indexOf("_") + 1); // avg

                if (section[yup] > Object.values(filter.GT)[0]) { // section[avg]
                    return true;
                } else {
                    return false;
                }

            } else if (filter.hasOwnProperty("LT")) {
                let yup = Object.keys(filter.LT)[0];
                yup = yup.substring(yup.indexOf("_") + 1);

                if (section[yup] < Object.values(filter.LT)[0]) {
                    return true;
                } else {
                    return false;
                }

            } else {
                let yup = Object.keys(filter.EQ)[0];
                yup = yup.substring(yup.indexOf("_") + 1);

                if (section[ yup] === Object.values(filter.EQ)[0]) {

                    return true;
                } else {
                    return false;
                }

            }
        } else if (filter.hasOwnProperty("NOT")) {

            // recursively call the same function with the same inputs but negated
            return (!this.isSectionValid(filter.NOT, section, id));

            // Check if it is an SComparison
        } else if (filter.hasOwnProperty("IS")) {

            // return InsightFacade.handleSComparisonHelper(filter.IS, section);

            let key: any = Object.keys(filter.IS)[0]; // courses_id
            // check if key is not invalid
            if (!this.validKeyHelper(key, id)) {
                throw new InsightError("Invalid key");
            }
            let value: any = Object.values(filter.IS)[0]; // courses_avg: VALUE
            // check if value is of right type
            if (typeof value !== "string") {
                throw new InsightError("Invalid type");
            }
            let actualRes: string = section[key.substring(key.indexOf("_") + 1)]; // section[id]
            // check each wildcard case
            if (value.includes("*")) {
                if (value.length === 1) {
                    return value === "*";
                    // *ell*, *ello, hell*
                } else if (value.length === 2 && value.startsWith("**")) {
                    return value === "**";
                } else if (value.startsWith("**") || value.endsWith("**")) {
                    throw new InsightError("Asteriks cannot be in the middle");
                } else if (value.startsWith("*") && value.endsWith("**")) {
                    throw new InsightError("Asteriks cannot be in the middle");
                } else if (value.startsWith("**") && value.endsWith("*")) {
                    throw new InsightError("Asteriks cannot be in the middle");
                } else if (value.startsWith("*") && value.endsWith("*")) {
                    return actualRes.includes(value.substring(1, value.length - 1));
                } else if (value.startsWith("*")) {
                    return actualRes.endsWith(value.substring(1));
                } else if (value.endsWith("*")) {
                    return actualRes.startsWith(value.substring(0, value.length - 1));
                } else {
                    // h**lo or h*llo === error
                    throw new InsightError("Asteriks cannot be in the middle");
                }
            }
            return value === actualRes;
        } else {
            // if no there is no filter
            if (filter.constructor === Object && Object.keys(filter).length === 0) {
                return false;
            } else {
                throw new InsightError("Did not match any of the keys");
            }
        }
    }
    public listDatasets(): Promise<InsightDataset[]> {
        let that = this;
        return new Promise<InsightDataset[]> ( function (resolve, reject) {
            let result: InsightDataset[] = [];

            for (let id of that.coursesMap.keys()) {
                let crows: number = that.coursesMap.get(id).length;
                result.push({id, kind: InsightDatasetKind.Courses, numRows: crows});
            }

            resolve(result);
        });
    }
}
