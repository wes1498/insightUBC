import Log from "../Util";
import {
    IInsightFacade,
    InsightCourse,
    InsightDataset,
    InsightDatasetKind,
    InsightError,
    NotFoundError,
    InsightRoom,
    IGeoResponse,
} from "./IInsightFacade";
import * as JSZip from "jszip";
import {JSZipObject} from "jszip";
import * as Path from "path";
import * as fs from "fs";
import * as parse5 from "parse5/lib";
import {fdatasyncSync} from "fs";
import * as http from "http";
import {cpus} from "os";

/**
 * This is the main programmatic entry point for the project.
 * Method documentation is in IInsightFacade
 *
 */

const indexLinkedBuildings: string[] = [];
export default class InsightFacade implements IInsightFacade {
    private coursesMap: Map<string, any>;
    private roomsMap: Map<string, any[]>;
    private linksMap: Map<string, any>;
    private realroomsMap: Map<string, any>;
    private linkArray: string[] = [];
    private  roomsMap2: Map<string, any>;
    private numberArray: string[] = [];
    private typeArray: string[] = [];
    private numArray: any[] = [];
    private  numStr: string = "";
    private addressArray: string[] = [];
    private addressMap: Map<string, any>;
    private nameMap: Map<any, any>;
    private numberMap: Map<any, any>;
    private nameArray1: string[] = [];
    private addressArray1: string[] = [];
    private numberArray1: string[][] = [];
    private typeArray1: string[][] = [];
    private furnArray1: string[][] = [];
    private sizeArray1: string[][] = [];
    private hrefArray1: string[][] = [];
    private mixArray1: string[][] = [];

    constructor() {
        Log.trace("InsightFacadeImpl::init()");
        this.coursesMap = new Map<string, any>();
        this.roomsMap = new Map<string, any[]>();
        this.linksMap = new Map<string, any>();
        this.roomsMap2 = new Map<string, any>();
        this.linkArray = [];
        this.numberArray = [];
        this.numArray = [];
        this.typeArray = [];
        this.numStr = "";
        this.addressArray = [];
        this.addressMap = new Map<string, any>();
        this.nameMap = new Map<any, any>();
        this.numberMap = new Map<any, any>();
        this.realroomsMap = new Map<string, any>();
        this.nameArray1 = [];
        this.addressArray1 = [];
        this.numberArray1 = [];
        this.typeArray1 = [];
        this.furnArray1 = [];
        this.sizeArray1 = [];
        this.hrefArray1 = [];
        this.mixArray1 = [];
    }
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
    private placeData(id: any, codearr: any) {
        let arr: any[] = [];
        let numarr: any[] = [];
        let hrefarr: any[] = [];
        let mixarr: any[] = [];
        let mixarr2: any[][] = [];
        let chunks: any[] = [];
        let adr: any [] = [];
        this.nameArray1.forEach((data1: any) => {
            // console.log(codearr[i]);
            // console.log(this.nameArray1[i]);
            arr.push(data1);
        });
        this.numberArray1.forEach((data1: any) => {
            numarr.push(data1);
        });
        arr.forEach((data1: any, i) => {
            let full = data1.replace(/.*:/, "");
            let short = data1.replace(/:.*/, "");
            this.hrefArray1.forEach((data2: any) => {
                data2.forEach((x: any) => {
                    let shortref = x.replace(/.*room/, "");
                    let shortref2 = short.replace("/", "");
                    if (x.indexOf(short) === 0) {
                        mixarr.push([data1, x, shortref2]);
                    }

                });
            });
        });
        let uniq = this.addressArray1.reduce(( a , b ) => {
            if (a.indexOf(b) < 0 ) {
                a.push(b);
            }
            return a;
        }, [] );
        uniq.forEach((x: any) => {
            let y = x;
            let h = y.replace(/[0-9]*.*/, x);
            adr.push(h);
        });
        console.log(adr);
        this.mixArray1.forEach((data1: any) => {
            data1.forEach((x: any, i: any) => {
                if (x.includes("http://students.ubc.ca/campus/discover/buildings-and-classrooms/room/")) {
                     mixarr2.push([x]);
                }
            });
            mixarr2.forEach((y: any) => {
               data1.forEach((t: any, c: any) => {
                  if (y[0] === data1[c]) {
                     chunks.push([data1[c], data1[c + 2], data1[c + 3], data1[c + 4]]);
                  }
               });
            });
        });
        arr.forEach((data1: any, i) => {
            let full = data1.replace(/.*:/, "");
            let short = data1.replace(/:.*/, "");
            let addr = data1[0].replace(/.*-h/, "" );
            let addr2 = "h" + addr;
            let addr3 = addr2.replace(/ /g, "%20");
            let url = "http://cs310.ugrad.cs.ubc.ca:11316/api/v1/project_e6y0b_s5c1b/" + addr3;
            let link = this.linksMap.get(short);
            let link2 = link[0].replace(/ /g, "%20");
            // console.log(link2);
        /*    this.getGeoInfo(url).then((data4: any) => {
                let latlon = JSON.parse(JSON.stringify(data4));
                let lat = latlon.lat;
                let lon = latlon.lon;
                // console.log(latlon);
            });*/
            chunks.forEach((data2: any) => {
                if (data2[0].indexOf(short) === 0) {
                    // console.log(data2[1]);
                    let regex = /.*\//;
                    let roomname = data2[0].replace(regex, "");
                    let obj = {
                        rooms_fullname: full,
                        rooms_shortname: short,
                        rooms_name: roomname,
                        rooms_seats: data2[1],
                        rooms_type: data2[3],
                        rooms_furniture: data2[2]};
                    // console.log(obj);
                }
            });
        });
   /*     while (this.mixArray1.length) {
            mixarr2.push([this.mixArray1.splice(0, 7)]);
        }*/
       // console.log(this.mixArray1);
        // console.log(mixarr2);
        // console.log(hrefarr);
                    // numarr.forEach((data2: any, i) => {
                    /*    data2.forEach((x: any) => {
                            // console.log(arr[]);
                            let obj = {
                                    rooms_fullname: full,
                                    rooms_shortname: short,
                                    rooms_name: x };
                            let roomsobj = JSON.stringify(obj);
                            this.roomsMap.get(id).push(roomsobj);
                        });*/
                    // });
        // console.log(this.roomsMap);
     /*  chunks.forEach((x: any) => {
            // console.log("As");
            console.log(x);
        });*/
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
                    this.roomsMap.get(id).push(roomsobj);
                    // resultsaver.push(roomsobj);
                });
                // return resolve(resultsaver);
            });
        });
    }
    // ADDS ROOM DATA FURNITURE ADDRESS FULL NAME ETC
    private roomData(content: any, id: string) {
        return new Promise<any>((resolve, reject) => {
            let resultsaver: any[] = [];
            let codearr: any[] = [];
            let roomCodes = Array.from(this.linksMap.keys());
            JSZip.loadAsync(content, {base64: true}).then((zipRooms: JSZip) => {
                roomCodes.forEach((code: any) => {
                    codearr.push(code);
                    // console.log(code);
                    let value = this.linksMap.get(code);
                    let subs = value[0].slice(2, 47);
                    resultsaver.push(zipRooms.file(subs).async("text").then((data: any) => {
                        this.getData(data, code);
                        this.getDataRoomNum(data, code);
                        // console.log(this.nameArray1);
                     /*   this.getData(data, code).then((data1: any) => {
                            this.getDataRoomNum(data, code).then((data2: any) => {
                                this.storeData(data1, data2, id, code).then((lon: any) => {
                                    // this.roomsMap.get(id).push(lon);
                                });
                            });
                        });*/
                    }));
            });
                // console.log(this.nameArray1);
                Promise.all(resultsaver).then(() => {
                    this.placeData(id, codearr);
                    // console.log(codearr);
                    // console.log(this.nameArray1);
                    // console.log(this.hrefArray1);
                /*  this.addressArray1.forEach((item) => {
                        console.log(item);
                    });*/
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
    private getData(data: any, code: string) {
        // return new Promise<any[]>((resolve, reject) => {
            let promises: any[] = [];
            let document: any = parse5.parse(data);
            let dataNodes: any[] = document.childNodes;
            dataNodes.forEach((childnode: any) => {
                if (childnode.tagName === "html") {
                    // this.getDatar(childnode, code);
                    // this.getDatar2(childnode, code);
                    // this.getDataMix(childnode, code);
                    this.getDataName(childnode, code);
                    this.getDataAddress(childnode, code);
                    // this.getDataCode(childnode, code);
                /*    this.getDataName(childnode, code).then((data2: any) => {
                        this.getDataAddress(childnode, code).then((data3: any) => {
                            return resolve([data2, data3]);
                        });
                    });*/
                    // this.getDataAddress(childnode, code);
                    // this.vie();
                }
            // });
        });
        // console.log(this.nameMap);
        // console.log(promises);
    }
    // GET DATA FROM ROOM NUMBER
    private getDataRoomNum(data: any, code: string) {
            let document: any = parse5.parse(data);
            let dataNodes: any[] = document.childNodes;
            dataNodes.forEach((childnode: any) => {
                if (childnode.tagName === "html") {
                    // this.getRoomNumber(childnode, code);
                    // this.getRoomType(childnode, code);
                    // this.getRoomFurn(childnode, code);
                    // this.getRoomSize(childnode, code);
                    // this.getRoomHref(childnode, code);
                    this.getAll(childnode, code);
                }
            });
    }
    // GET DATA NAME
    private getDataName(data: any, code: string) {
        // return new Promise<any[]>((resolve, reject) => {
            let dataNodes: any[] = data.childNodes;
            dataNodes.forEach((childnode: any) => {
                if (childnode.tagName === null || childnode.tagName === undefined) {
                    // console.log("undef");
                } else if (childnode.tagName !== "h2") {
                    this.getDataName(childnode, code);
                } else {
                    // console.log(this.getDataName2(childnode, code));
                    this.getDataName2(childnode, code);
                }
            });
        // });
    }
    // GET DATA NAME
    private getDataName2(data: any, code: string) {
       // return new Promise<any[]>((resolve, reject) => {
            let dataNodes: any[] = data.childNodes;
            dataNodes.forEach((childnode: any) => {
                if (childnode.tagName === "span") {
                    // console.log(childnode);
                    this.getDataName3(childnode, code);
                }
            });
        // });
    }
    // GET DATA NAME
    private getDataName3(data: any, code: string) {
       // return new Promise<any[]>((resolve, reject) => {
            let dataNodes: any[] = data.childNodes;
            dataNodes.forEach((childnode: any) => {
                if (childnode.nodeName === "#text") {
                    // this.nameMap.set(code, childnode.value);
                    // console.log(code + ": " + childnode.value);
                    // console.log("line");
                    // console.log(childnode.value);
                    this.nameArray1.push(code + ":" + childnode.value);
                }
            });
       // });
    }
    // GET DATA ADDRESS
    // GET DATA ADDRESS
    private getDataAddress(data: any, code: string) {
            let dataNodes: any[] = data.childNodes;
            let classVal = "building-info";
            dataNodes.forEach((childnode: any) => {
                if (childnode.tagName === null || childnode.tagName === undefined) {
                    // console.log("undef");
                } else if (childnode.tagName !== "div") {
                    this.getDataAddress(childnode, code);
                } else {
                    if (typeof childnode.attrs !== "undefined") {
                        childnode.attrs.forEach((at: any) => {
                            if (childnode.attrs.length >= 1) {
                                if (at.value === classVal) {
                                    // console.log(childnode);
                                    this.getDataAddress5(childnode, code);
                                } else {
                                    this.getDataAddress(childnode, code);
                                }
                            }
                        });
                    }
                }
        });
    }
    // GET DATA ADDRESS
    private getDataAddress5(data: any, code: string) {
            let dataNodes: any[] = data.childNodes;
            dataNodes.forEach((childnode: any) => {
                if (childnode.tagName === "div") {
                    this.getDataAddress6(childnode, code);
                }
            });
    }
    // GET DATA ADDRESS
    private getDataAddress6(data: any, code: string) {
            let dataNodes: any[] = data.childNodes;
            dataNodes.forEach((childnode: any) => {
                if (childnode.tagName === "div") {
                    // console.log(childnode);
                    this.getDataAddress7(childnode, code);
                }
            });
    }
    // GET DATA ADDRESS
    private getDataAddress7(data: any, code: string) {
            let dataNodes: any[] = data.childNodes;
            dataNodes.forEach((childnode: any) => {
                if (childnode.nodeName === "#text") {
                    if (childnode.value.substring(0, 8) === "Building") {
                        // nda
                    } else {
                        this.addressArray1.push(childnode.value);
                    }
                }
            });
    }
    private getAll(data: any, code: string) {
        let dataNodes: any[] = data.childNodes;
        let res: any[] = [];
        dataNodes.forEach((childnode: any) => {
            if (childnode.tagName === null || childnode.tagName === undefined || childnode.length === 0) {
                // console.log("undef");
            } else if (childnode.tagName !== "tr") {
                this.getAll(childnode, code);
            } else {
                res.push(childnode);
            }
        });
        if (res.length !== 0) {
            if (res === undefined) {
                // c
            } else {
                this.getAll2(res, code);
            }
        }
    }
    private getAll2(prom: any, code: string) {
        let c1 = "odd";
        let c2 = "even";
        let c3 = "even views-row-last";
        let c4 = "odd views-row-last";
        let c5 = "even views-row-first";
        let c6 = "odd views-row-first";
        let res: any[] = [];
        prom.forEach((datatr: any) => {
            if (typeof datatr.attrs !== "undefined") {
                datatr.attrs.forEach((at: any) => {
                    if (datatr.attrs.length >= 1) {
                        let x = at.value;
                        // console.log(x);
                        if (x === c1 || x === c2 || x === c3 || x === c4 || x === c5 || x === c6) {
                            // console.log(childnode);
                            // console.log(x);
                            res.push(datatr);
                        }
                    }
                });
            }
        });
        if (res.length !== 0) {
            if (res === undefined) {
                // c
            } else {
                // console.log(res);
                this.getAll3(res, code);
            }
        }
    }
    private getAll3(prom: any, code: string) {
        let l1 = "views-field views-field-field-room-number";
        let l2 = "views-field views-field-field-room-capacity";
        let l3 = "views-field views-field-field-room-type";
        let l4 = "views-field views-field-field-room-furniture";
        let promises: any[] = [];
        let promises2: any[] = [];
        prom.forEach((datar: any) => {
            let x = datar.childNodes;
            //  console.log(x);
            x.forEach((dats: any) => {
                if (dats.tagName === "td") {
                    // console.log("made it");
                    if (typeof dats.attrs !== "undefined") {
                        dats.attrs.forEach((atr: any) => {
                            if (atr.value === l1 || atr.value  === l2 || atr.value  === l3 || atr.value  === l4) {
                                // console.log(code);
                                // console.log(atr);
                                // console.log("eyyyyyyyyyy");
                                // console.log(x);
                                promises.push(dats);
                                // promises.push(dats);
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
                 this.getAll4(promises, code);
            }
        }
    }
    private getAll4(prom: any, code: string) {
        let res: any[] = [];
        prom.forEach((data2: any) => {
            let hreftex = "";
            let x = data2.childNodes;
            x.forEach((cnodes: any) => {
                if (cnodes.tagName === "a") {
                    let y = cnodes.attrs;
                    let z = cnodes.childNodes;
                    z.forEach((tex2: any) => {
                        if (tex2.nodeName === "#text") {
                            // console.log("newwwwwwwwwwww val");
                            // console.log(tex.value);
                            res.push(code + "-" + tex2.value);
                        }
                    });
                    y.forEach((tex: any) => {
                        if (tex.name === "href") {
                            // console.log("newwwwwwwwwwww val");
                            // console.log(tex.value);
                             res.push(code + "-" + tex.value);
                        }
                    });
                } else {
                    let str = cnodes.value.replace(/\s/g, "");
                    res.push(code + ":" + str);
                }
            });
        });
        if (res.length !== 0) {
            if (res === undefined) {
                // c
            } else {
                res.forEach((x: any) => {
                    let regex = /.*:/;
                    if (x === regex) {
                        // console.log("pedo");
                    } else {
                        // console.log(x);
                    }
                });
                // console.log(res);
                this.mixArray1.push(res);
            }
        }
    }
    private getRoomNumber(data: any, code: string) {
            let dataNodes: any[] = data.childNodes;
            let res: any[] = [];
            dataNodes.forEach((childnode: any) => {
                if (childnode.tagName === null || childnode.tagName === undefined || childnode.length === 0) {
                    // console.log("undef");
                } else if (childnode.tagName !== "tr") {
                    this.getRoomNumber(childnode, code);
                } else {
                    res.push(childnode);
                }
            });
            if (res.length !== 0) {
                if (res === undefined) {
                   // c
                    } else {
                    this.getRoomNumber2(res, code);
                }
            }
    }
    private getRoomNumber2(prom: any, code: string) {
        let c1 = "odd";
        let c2 = "even";
        let c3 = "even views-row-last";
        let c4 = "odd views-row-last";
        let c5 = "even views-row-first";
        let c6 = "odd views-row-first";
        let res: any[] = [];
        prom.forEach((datatr: any) => {
            if (typeof datatr.attrs !== "undefined") {
                datatr.attrs.forEach((at: any) => {
                    if (datatr.attrs.length >= 1) {
                        let x = at.value;
                        // console.log(x);
                        if (x === c1 || x === c2 || x === c3 || x === c4 || x === c5 || x === c6) {
                            // console.log(childnode);
                            // console.log(x);
                            res.push(datatr);
                        }
                    }
                });
            }
        });
        if (res.length !== 0) {
            if (res === undefined) {
                // c
            } else {
                // console.log(res);
                this.getRoomNumber3(res, code);
            }
        }
    }
    private getRoomNumber3(prom: any, code: string) {
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
                    this.getRoomNumber4(promises, code);
                }
            }
    }
    private getRoomNumber4(prom: any, code: string): any {
            let res: any[] = [];
            prom.forEach((data2: any) => {
                let x = data2.childNodes;
                x.forEach((cnodes: any) => {
                    if (cnodes.tagName === "a") {
                     let y = cnodes.childNodes;
                     y.forEach((tex: any) => {
                         if (tex.nodeName === "#text") {
                             // console.log("newwwwwwwwwwww val");
                             // console.log(tex.value);
                             res.push(code + "-" + tex.value);
                         }
                     });
                   }
                });
            });
            if (res.length !== 0) {
                if (res === undefined) {
                    // c
                } else {
                   // console.log(res);
                    this.numberArray1.push(res);
                }
            }
    }
    private getRoomType(data: any, code: string) {
            let dataNodes: any[] = data.childNodes;
            let res: any[] = [];
            dataNodes.forEach((childnode: any) => {
                if (childnode.tagName === null || childnode.tagName === undefined || childnode.length === 0) {
                    // console.log("undef");
                } else if (childnode.tagName !== "tr") {
                    // Promise.all()
                    this.getRoomType(childnode, code);
                } else {
                    res.push(childnode);
                }
            });
            if (res.length !== 0) {
                if (res === undefined) {
                    // c
                } else {
                    // console.log(res);
                    this.getRoomType2(res, code);
                }
            }
    }
    private getRoomType2(prom: any, code: string) {
            //  let dataNodes: any[] = data.childNodes;
            let c1 = "odd";
            let c2 = "even";
            let c3 = "even views-row-last";
            let c4 = "odd views-row-last";
            let c5 = "even views-row-first";
            let c6 = "odd views-row-first";
            let res: any[] = [];
            prom.forEach((datatr: any) => {
                if (typeof datatr.attrs !== "undefined") {
                    datatr.attrs.forEach((at: any) => {
                        if (datatr.attrs.length >= 1) {
                            let x = at.value;
                            // console.log(x);
                            if (x === c1 || x === c2 || x === c3 || x === c4 || x === c5 || x === c6 ) {
                                // console.log(childnode);
                                // console.log(x);
                                res.push(datatr);
                            }
                        }
                    });
                }
            });
            if (res.length !== 0) {
                if (res === undefined) {
                    // c
                } else {
                    // console.log(res);
                    this.getRoomType3(res, code);
                }
            }
    }
    private getRoomType3(prom: any, code: string) {
            // let dataNodes: any[] = data.childNodes;
            // let classVal = "views-field views-field-field-room-number";
            let classVal2 = "views-field views-field-field-room-type";
            let res: any[] = [];
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
                                    res.push(dats);
                                }
                            });
                        }
                    }
                });
            });
            if (res.length !== 0) {
                if (res === undefined) {
                    // c
                } else {
                    // console.log(res);
                    this.getRoomType4(res, code);
                }
            }
    }
    private getRoomType4(prom: any, code: string) {
            let res: any[] = [];
            prom.forEach((data2: any) => {
               let x = data2.childNodes;
               x.forEach((cnodes: any) => {
                   let str = cnodes.value.replace(/\s/g, "");
                   res.push(code + ": " + str);
               });
            });
            if (res.length !== 0) {
                if (res === undefined) {
                    // c
                } else {
                    // console.log(res);
                    this.typeArray1.push(res);
                }
            }
    }
    private getRoomFurn(data: any, code: string) {
            let dataNodes: any[] = data.childNodes;
            let res: any[] = [];
            dataNodes.forEach((childnode: any) => {
                if (childnode.tagName === null || childnode.tagName === undefined || childnode.length === 0) {
                    // console.log("undef");
                } else if (childnode.tagName !== "tr") {
                    // Promise.all()
                    this.getRoomFurn(childnode, code);
                } else {
                    res.push(childnode);
                }
            });
            if (res.length !== 0) {
                if (res === undefined) {
                    // c
                } else {
                    // console.log(res);
                    this.getRoomFurn2(res, code);
                }
            }
    }
    private getRoomFurn2(prom: any, code: string) {
        //  let dataNodes: any[] = data.childNodes;
        let c1 = "odd";
        let c2 = "even";
        let c3 = "even views-row-last";
        let c4 = "odd views-row-last";
        let c5 = "even views-row-first";
        let c6 = "odd views-row-first";
        let res: any[] = [];
        prom.forEach((datatr: any) => {
            if (typeof datatr.attrs !== "undefined") {
                datatr.attrs.forEach((at: any) => {
                    if (datatr.attrs.length >= 1) {
                        let x = at.value;
                        // console.log(x);
                        if (x === c1 || x === c2 || x === c3 || x === c4 || x === c5 || x === c6) {
                            // console.log(childnode);
                            // console.log(x);
                            res.push(datatr);
                        }
                    }
                });
            }
        });
        if (res.length !== 0) {
            if (res === undefined) {
                // c
            } else {
                // console.log(res);
                this.getRoomFurn3(res, code);
            }
        }
    }
    private getRoomFurn3(prom: any, code: string) {
            // let dataNodes: any[] = data.childNodes;
            // let classVal = "views-field views-field-field-room-number";
            let classVal2 = "views-field views-field-field-room-furniture";
            let res: any[] = [];
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
                                    res.push(dats);
                                }
                            });
                        }
                    }
                });
            });
            if (res.length !== 0) {
                if (res === undefined) {
                    // c
                } else {
                    // console.log(res);
                    this.getRoomFurn4(res, code);
                }
            }
    }
    private getRoomFurn4(prom: any, code: string) {
            let res: any[] = [];
            prom.forEach((data2: any) => {
                let x = data2.childNodes;
                x.forEach((cnodes: any) => {
                    let str = cnodes.value.replace(/\s/g, "");
                    res.push(code + ": " + str);
                });
            });
            if (res.length !== 0) {
                if (res === undefined) {
                    // c
                } else {
                    // console.log(res);
                    this.furnArray1.push(res);
                }
            }
    }
    private getRoomSize(data: any, code: string) {
            let dataNodes: any[] = data.childNodes;
            let res: any[] = [];
            dataNodes.forEach((childnode: any) => {
                if (childnode.tagName === null || childnode.tagName === undefined || childnode.length === 0) {
                    // console.log("undef");
                } else if (childnode.tagName !== "tr") {
                    // Promise.all()
                    this.getRoomSize(childnode, code);
                } else {
                    res.push(childnode);
                }
            });
            if (res.length !== 0) {
                if (res === undefined) {
                    // c
                } else {
                    // console.log(res);
                    this.getRoomSize2(res, code);
                }
            }
    }
    private getRoomSize2(prom: any, code: string) {
            //  let dataNodes: any[] = data.childNodes;
            let c1 = "odd";
            let c2 = "even";
            let c3 = "even views-row-last";
            let c4 = "odd views-row-last";
            let c5 = "even views-row-first";
            let c6 = "odd views-row-first";
            let res: any[] = [];
            prom.forEach((datatr: any) => {
                if (typeof datatr.attrs !== "undefined") {
                    datatr.attrs.forEach((at: any) => {
                        if (datatr.attrs.length >= 1) {
                            let x = at.value;
                            // console.log(x);
                            if (x === c1 || x === c2 || x === c3 || x === c4 || x === c5 || x === c6 ) {
                                // console.log(childnode);
                                // console.log(x);
                                res.push(datatr);
                            }
                        }
                    });
                }
            });
            if (res.length !== 0) {
                if (res === undefined) {
                    // c
                } else {
                    // console.log(res);
                    this.getRoomSize3(res, code);
                }
            }
    }
    private getRoomSize3(prom: any, code: string) {
            // let dataNodes: any[] = data.childNodes;
            // let classVal = "views-field views-field-field-room-number";
            let classVal2 = "views-field views-field-field-room-capacity";
            let res: any[] = [];
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
                                    res.push(dats);
                                }
                            });
                        }
                    }
                });
            });
            if (res.length !== 0) {
                if (res === undefined) {
                    // c
                } else {
                    // console.log(res);
                    this.getRoomSize4(res, code);
                }
            }
    }
    private getRoomSize4(prom: any, code: string) {
            let res: any[] = [];
            prom.forEach((data2: any) => {
                let x = data2.childNodes;
                x.forEach((cnodes: any) => {
                    let str = cnodes.value.replace(/\s/g, "");
                    res.push(code + ":" + str);
                });
            });
            if (res.length !== 0) {
                if (res === undefined) {
                    // c
                } else {
                    // console.log(res);
                    this.sizeArray1.push(res);
                }
            }
    }
    private getRoomHref(data: any, code: string) {
            let dataNodes: any[] = data.childNodes;
            let res: any[] = [];
            dataNodes.forEach((childnode: any) => {
                if (childnode.tagName === null || childnode.tagName === undefined || childnode.length === 0) {
                    // console.log("undef");
                } else if (childnode.tagName !== "tr") {
                    // Promise.all()
                    this.getRoomHref(childnode, code);
                } else {
                    res.push(childnode);
                }
            });
            if (res.length !== 0) {
                if (res === undefined) {
                    // c
                } else {
                    // console.log(res);
                    this.getRoomHref2(res, code);
                }
            }
    }
    private getRoomHref2(prom: any, code: string) {
            //  let dataNodes: any[] = data.childNodes;
            let c1 = "odd";
            let c2 = "even";
            let c3 = "even views-row-last";
            let c4 = "odd views-row-last";
            let c5 = "even views-row-first";
            let c6 = "odd views-row-first";
            let res: any[] = [];
            prom.forEach((datatr: any) => {
                if (typeof datatr.attrs !== "undefined") {
                    datatr.attrs.forEach((at: any) => {
                        if (datatr.attrs.length >= 1) {
                            let x = at.value;
                            // console.log(x);
                            if (x === c1 || x === c2 || x === c3 || x === c4 || x === c5 || x === c6 ) {
                                // console.log(childnode);
                                // console.log(x);
                                res.push(datatr);
                            }
                        }
                    });
                }
            });
            if (res.length !== 0) {
                if (res === undefined) {
                    // c
                } else {
                    // console.log(res);
                    this.getRoomHref3(res, code);
                }
            }
    }
    private getRoomHref3(prom: any, code: string) {
            // let dataNodes: any[] = data.childNodes;
            let classVal = "views-field views-field-field-room-number";
            let classVal2 = "views-field views-field-field-room-capacity";
            let res: any[] = [];
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
                                    res.push(dats);
                                }
                            });
                        }
                    }
                });
            });
            if (res.length !== 0) {
                if (res === undefined) {
                    // c
                } else {
                    // console.log(res);
                    this.getRoomHref4(res, code);
                }
            }
    }
    private getRoomHref4(prom: any, code: string) {
            let classVal = "views-field views-field-field-room-number";
            let classVal2 = "views-field views-field-field-room-type";
            let res: any[] = [];
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
                        let y = cnodes.attrs;
                        y.forEach((tex: any) => {
                            if (tex.name === "href") {
                                // console.log("newwwwwwwwwwww val");
                                // console.log(tex.value);
                                res.push(code + ": " + tex.value);
                            }
                        });
                    }
                });
            });
            if (res.length !== 0) {
                if (res === undefined) {
                    // c
                } else {
                    this.hrefArray1.push(res);
                }
            }
    }
    public addDataset(id: string, content: string, kind: InsightDatasetKind): Promise<string[]> {
        return new Promise<string[]>((resolve, reject) => {
            let promises: any[] = [];
            let filter: string = "A";
            if (this.coursesMap.has(id)) {
                filter = "has ID";
            } else if (!id || id.length === 0 || id === "") {
                filter = "Not valid";
            }
            switch (filter) {
                case "Not valid": {
                    return reject(new InsightError("Invalid Id"));
                }
                case "has ID": {
                    return reject(new InsightError("Id is already added"));
                }
                default: {
                    this.coursesMap.set(id, []);
                }
            }
            if (kind === InsightDatasetKind.Rooms) {
                JSZip.loadAsync(content, {base64: true}).then((zipRooms: JSZip) => {
                    // console.log(zipRooms.file("index.htm"));
                    // console.log(zipRooms);
                    // console.log(zipRooms.file("index.htm").async("text"));
                        promises.push(zipRooms.file("index.htm").async("text").then( (data: any) => {
                             this.roomsMap.set(id, []);
                             this.addRoom(data, id);
                             this.roomData(content, id).then((x: any) => {
                                 // console.log(this.roomsMap);
                             });
                            // console.log(this.roomsMap);
                        }));
                });
                Promise.all(promises).then((result) => {
                    // co
                });
            }
            JSZip.loadAsync(content, {base64: true}).then((zip: JSZip) => {
                // console.log(zip);
            /*    this.addPromises(zip, id).then((data: string[]) => {
                    // console.log(data);
                    res = data;
                    // console.log(this.coursesMap.get(id));
                });*/
                // console.log(this.coursesMap.get(id));
                // console.log(this.coursesMap.entries());
                // console.log(zip.files);
                // console.log(zip);
                if (zip.length < 1) {
                    return reject(new InsightError("Invalid Id"));
                }
                if (kind === InsightDatasetKind.Courses) {
                    zip.forEach(((relativePath, file: JSZipObject) => {
                        promises.push(file.async("text").then((data: string) => {
                            // console.log(data);
                            this.addCourse(data, id);
                            /*         .then((data2: any) => {
                                     // console.log(res);
                                     return data2;
                                 }).catch((x) => {
                                     // console.log("add course did not work " + x);
                                     throw new Error("add course fucked up " + x);
                                     // return reject(new InsightError("add course did not work " + x));
                                 });*/
                            // console.log(res);
                            // console.log(this.coursesMap); // save the course info a
                        }).catch((e) => {
                            return reject(new InsightError("1 " + e));
                        }));
                    }));
                } else {
                    return reject(new InsightError("Desired folder for the dataset kind does not exist"));
                }
                Promise.all(promises).then((result) => {
                    // console.log(res);
                    let arr: string[] = this.coursesMap.get(id);
                    // console.log(arr);
                    // console.log(this.coursesMap.get(id));
                    let dat = Object.assign({}, arr);
                    fs.writeFile("msg" + id + ".txt", JSON.stringify(arr), (e) => {
                        if (e) {
                            console.log("reeeeee" + e + " error");
                        } else {
                            return Promise.resolve(arr);
                        }
                    });
                    return result;
                });
            }).catch((e) => {
                return reject(new InsightError("zip " + e));
            });
        });
}
  /*  private checker(id: string) {
       // console.log(this.coursesMap.get("coursesOne"));
       // console.log(this.coursesMap.get("coursesTwo"));
       // console.log(this.coursesMap.get("coursesDouble"));
        /!*let arr: string[] = this.coursesMap.get(id);
        console.log(arr);
        let dat = Object.assign({}, arr);
        fs.writeFile("msg" + id + ".txt", arr, (e) => {
            if (e) {
             console.log(e + " error");
            } else {
                return Promise.resolve(JSON.stringify(dat));
            }
        });*!/
    }*/
    private addCourse(data: string, datasetId: string) {
        // return new Promise<any[]>((resolve, reject) => {
            // console.log(data);
            // let dat = Object.assign({}, data);
            // console.log(dat);
            // let parsedData;
   /*         if (data.length === 0) {
               // console.log("this is the fucker: " + data);
                return reject(new InsightError("peaceeeeeee " + data.toString() + "noting"));
            }*/
            // console.log(data);
            try {
                            // console.log(data);
                        let parsedData = JSON.parse(data);
                            // console.log(parsedData);
                        let filterPromises: any[] = [];
                        if (parsedData.result.length === 0) {
                            throw new Error("0 section.");
                    }
                        this.coursesMap.set(datasetId, []);
                        let dataObj: object[] = parsedData.result;
                        dataObj.forEach((section: any) => {
                            this.filters([section], datasetId).then((stuff3: any) => {
                                let dataObjParse: any[] = stuff3;
                                // this.coursesMap.set(datasetId, []);
                                this.coursesMap.get(datasetId).push(dataObjParse);
                                // console.log(this.coursesMap.get(datasetId));
                                // console.log(datasetId);
                                // return resolve(dataObjParse);
                            }).catch((e) => {
                                throw new Error("0 reject");
                            });
                        });
                    } catch (e) {
                        // console.log(e);
                        throw new Error("errirrr " + e );
                    }
    // });
    }
    // filterPromises[section] = stuff3;
    // this.coursesMap.get(datasetId).push(stuff3);
    // console.log(stuff3);
            // console.log(this.coursesMap.get(datasetId));
            // this.checker(datasetId);
           // return resolve(this.coursesMap.get(datasetId));
            /*  for (let i = 0; i <= dataObj.length ; i++) {
                  let dataObj2: object[] = [dataObj[i]];
                  if (dataObj[i].hasOwnProperty(undefined)) {
                      console.log("reeeeeeeeeee");
                  }
                  let stuff2 = this.filters(dataObj2);
                  filterPromises[i] = [stuff2];
                  this.coursesMap.set(datasetId, stuff2);
                  // console.log(stuff2);
              }
              console.log(this.coursesMap.entries());*/
            // return resolve(filterPromises);
    private  filters (someone: object[], datasetId: string): Promise<InsightCourse> {
        return new Promise<InsightCourse>((resolve, reject) => {
           /* if (someone === undefined) {
                return reject(new InsightError("it is undefined"));
            }*/
           try {
               someone.map((sect: any) => {
                   let subject: string = sect.Subject as string;
                   let year: number = sect.Year as number;
                   let professor: string = sect.Professor as string;
                   let id: string = sect.Course as string;
                   let avg: number = sect.Avg as number;
                   let title: string = sect.Title as string;
                   let pass: string = sect.Pass as string;
                   let fail: string = sect.Fail as string;
                   let uuid: string = sect.id.toString() as string;
                   let audit: number = sect.Audit as number;

                   let filteredCourse: InsightCourse = {
                       courses_dept: subject,
                       courses_id: id,
                       courses_avg: avg,
                       courses_instructor: professor,
                       courses_title: title,
                       courses_pass: pass,
                       courses_fail: fail,
                       courses_uuid: uuid,
                       courses_audit: audit,
                       courses_year: year,
                   };
                   // this.coursesMap.get(datasetId).push(filteredCourse);
                   return resolve(filteredCourse);
               });
           } catch (e) {
               console.log(e);
               return reject("Error");
           }
    });
    }
/*    private addCourse(data: string, id: string) {
        let vary = JSON.parse(data);
        let dataArray: object[] = vary.result;
        let sub: string = dataArray.Subject as string;
        console.log(dataArray.sub);
    }*/

    /*public addDataset(id: string, content: string, kind: InsightDatasetKind): Promise<string[]> {
        return new Promise<string[]>((resolve, reject) => {
            let entries: any = [];
            JSZip.loadAsync(content, {base64: true}).then((zip: JSZip) => {
                zip.folder("courses/").forEach(((relativePath, fileObject: JSZipObject) => {
                    entries.push(fileObject.async("text").then((data: string) => {
                       // this.addCourse(data, id); // save the course info a
                        console.log(fileObject);
                        return Promise.resolve(entries);
                    }).catch((e) => {
                        return Promise.reject(new InsightError("Error processing encoded course data " + e));
                    }));
                }));
               // return Promise.resolve(entries);
            });
            // return Promise.resolve(entries);
    });
    }*/
    public removeDataset(id: string): Promise<string> {// no changes needed
        return new Promise<string>((resolve, reject) => {
            if (id === "") {
                return reject(new NotFoundError("Invalid ID"));
            } else if (id === null || !id) {
                return reject(new InsightError("Invalid ID"));
            } else if (!this.coursesMap.has(id)) {
                return reject(new NotFoundError("Not found"));
            }
       /*     if (this.coursesMap.has(id)) {
                this.removeFromMemory(id, InsightDatasetKind.Courses).then((succ) => {
                    return resolve(id);
                }).catch((err) => {
                    return reject(new NotFoundError("error :" +  err ));
                });
            } else if (!this.coursesMap.has(id)) {
                return reject(new NotFoundError("Not found"));
            }*/
            this.coursesMap.delete(id);
            return resolve (id);
        });
    }
    private removeFromMemory(id: string, kind: InsightDatasetKind): Promise<boolean> {
        return new Promise<boolean>((resolve, reject) => {
            fs.readdir(kind.toString(), (err, files) => {
                if (err) {
                    // console.log(err + "1");
                    return reject("error in deleting: " + err);
                } else {
                    if (files.includes(id)) {
                        const pathy = Path.join(kind, id);
                        fs.unlink(pathy, (err2) => {
                            if (err2) {
                                //  console.log(err2);
                                reject(false);
                            } else {
                                resolve(true);
                            }
                        });
                    } else {
                        reject(false);
                    }
                }
            });
        });
    }

    public performQuery(query: any): Promise<any[]> {
        return Promise.reject("Not implemented.");
    }
    public listDatasets(): Promise<InsightDataset[]> {
        return new Promise<InsightDataset[]> ( (resolve, reject) => {
            let result: InsightDataset[] = [];

            for (let id of this.coursesMap.keys()) {
                let crows: number = this.coursesMap.get(id).length;
                // console.log(crows);
                result.push({id, kind: InsightDatasetKind.Courses, numRows: crows});
            }

            resolve(result);
        });
    }
}
