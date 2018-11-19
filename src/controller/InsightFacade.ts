import Log from "../Util";
import {
    IGeoResponse,
    IInsightFacade,
    InsightDataset,
    InsightDatasetKind,
    InsightError,
    InsightFilter, NotFoundError
} from "./IInsightFacade";
import * as JSZip from "jszip";
import {JSZipObject} from "jszip";
import * as fs from "fs";
import {Decimal} from "decimal.js";
import * as http from "http";
import * as Path from "path";

const parse5 = require("parse5");
const COURSES = InsightDatasetKind.Courses;
const ROOMS = InsightDatasetKind.Rooms;
/**
 * This is the main programmatic entry point for the project.
 * Method documentation is in IInsightFacade
 *
 */

export default class InsightFacade implements IInsightFacade {
    private static instancefacade: InsightFacade = new InsightFacade();
    private mydatasets: Map<string, any[]>;
    private coursesMap: Map<string, any>;
    private roomsMap: Map<string, any[]>;
    private linksMap: Map<string, any>;
    private nameArray1: string[] = [];
    private addressArray1: string[] = [];
    private numberArray1: string[][] = [];
    private typeArray1: string[][] = [];
    private furnArray1: string[][] = [];
    private sizeArray1: string[][] = [];
    private hrefArray1: string[][] = [];
    private mixArray1: string[][] = [];
    private latlonArray1: string[][] = [];
    private kepler: any[] = [];

    constructor() {
        Log.trace("InsightFacadeImpl::init()");
        this.mydatasets = new Map<string, any>();
        this.coursesMap = new Map<string, any>();
        this.roomsMap = new Map<string, any[]>();
        this.linksMap = new Map<string, any>();
        this.nameArray1 = [];
        this.addressArray1 = [];
        this.numberArray1 = [];
        this.typeArray1 = [];
        this.furnArray1 = [];
        this.sizeArray1 = [];
        this.hrefArray1 = [];
        this.mixArray1 = [];
        this.latlonArray1 = [];
        this.kepler = [];
    }

    public static getInstance() {
        return InsightFacade.instancefacade;
    }

    // GETS LINKS FROM INDEX.HTM IT ALSO GETS SHORT NAME
    private addRoomLinks(data: any, code: string) {
        // let doc: any = parse5.parse(data);
        // let data2 = data.async("text");

        let document: any = parse5.parse(data);
        let dataNodes: any[] = document.childNodes;
        dataNodes.forEach((childnode: any) => {
            if (childnode.tagName === "html") {
                // this.addRoom2(childnode);
                this.getDataLinks(childnode);
            }
        });
    }

    private getDataLinks(data: any) {
        let dataNodes: any[] = data.childNodes;
        let classVal = "building-info";
        dataNodes.forEach((childnode: any) => {
            if (childnode.tagName === null || childnode.tagName === undefined) {
                // whatevs
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
                // shit
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

        let sub = data[0].substring(43, 47);

        let elem = data[0];
        this.linksMap.set(sub, data);
        let merged = [].concat.apply([], data);

        // this.check();
    }

    private placeData(id: any, codearr: any): any {
        return new Promise<any>((resolve, reject) => {
            let that = this;
            let arr: any[] = [];
            let numarr: any[] = [];
            let hrefarr: any[] = [];
            let mixarr: any[] = [];
            let mixarr2: any[][] = [];
            let chunks: any[] = [];
            let adr: any [] = [];
            let mixarr3: any[] = [];
            let resul: any[] = [];
            this.nameArray1.forEach((data1: any) => {
                arr.push(data1);
                // console.log(data1);
            });
            this.numberArray1.forEach((data1: any) => {
                numarr.push(data1);
                // console.log(data1);
            });
            // getting all the rooms stuff together
            arr.forEach((data1: any, i) => {
                let full = data1.replace(/.*:/, "");
                let short = data1.replace(/:.*!/, "");
                this.hrefArray1.forEach((data2: any) => {
                    data2.forEach((x: any) => {
                        let shortref = x.replace(/.*room/, "");
                        let shortref2 = short.replace("/", "");
                        if (x.indexOf(short) === 0) {
                            mixarr.push([data1, x, shortref2]);
                            // console.log(data1);
                        }

                    });
                });
            });
            // getting only the correct addresses
            // console.log(this.addressArray1);
            let uniq = this.addressArray1.reduce((a, b) => {
                if (a.indexOf(b) < 0) {
                    a.push(b);
                }
                return a;
            }, []);
            uniq.forEach((x: any) => {
                if (x.includes("Formerly known") || x.includes("Opening hours") || x.includes("TBD")) {
                    // console
                } else {
                    adr.push(x);
                }
            });
            // putting all room stuff together
            this.mixArray1.forEach((data1: any) => {
                // console.log(data1);
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
            // Puting address and names together
            arr.forEach((data1: any, i) => {
                // console.log(data1);
                let full = data1.replace(/.*:/, "");
                let short = data1.replace(/:.*/, "");
                let adrarr: any[] = [];
                adr.forEach((x: any) => {
                    let sub = x.substring(0, 4);
                    let sub2 = x.substring(0, 3);
                    if (short === sub || short === sub2) {
                        // console.log(sub2 + "------" + sub);
                        mixarr3.push(data1 + " -- " + x);
                        // console.log(data1 + " -- ");
                    }
                });
            });
            // console.log(mixarr3)
            that.putData(mixarr3, chunks, id).then((x: any) => {
                // console.log(x);
                return resolve(x);
                // console.log(x);
            });
        });
    }

    private getGeo(url: string, fu2: any, st: any, r2: any, r3: any, a2: any, s2: any, te: any, fn: any, hf: any):
        Promise<IGeoResponse> {
            return new Promise<IGeoResponse>((resolve, reject) => {
                http.get(url, (result) => {
                    result.setEncoding("utf8");
                    let body = "";
                    result.on("data", (chunk) => {
                        body += chunk;
                    }).on("error", (err) => {
                        return reject("GEO ERROR: " + err);
                    });
                    result.on("end", () => {
                        let resultVal: IGeoResponse = JSON.parse(body);
                        let latlon = JSON.parse(JSON.stringify(resultVal));
                        let lati = latlon.lat;
                        let longi = latlon.lon;
                        let obj = {
                            rooms_fullname: fu2,
                            rooms_shortname: st,
                            rooms_number: r2,
                            rooms_name: r3,
                            rooms_address: a2,
                            rooms_lat: lati,
                            rooms_lon: longi,
                            rooms_seats: s2,
                            rooms_type: te,
                            rooms_furniture: fn,
                            rooms_href: "http:" + hf
                        };
                        let roomsobj = JSON.parse(JSON.stringify(obj));
                        return resolve(roomsobj);
                    });
                    // console.log("caught it here");
                    // return reject("GEO ERROR: ");
                });
            });
    }

    private putData(mixarr3: any, chunks: any, id: any): any {
        return new Promise<any>((resolve, reject) => {
            let that = this;
            let latlonarr: any[] = [];
            let latlonarr2: any[] = [];
            let fx: any[] = [];
            let short = "";
            let full = "";
            let full2 = "";
            // console.log(mixarr3);
            mixarr3.forEach((data1: any) => {
                // console.log(data1);
                short = data1.replace(/:.*/, "");
                let addrs = data1.replace(/.* --/, "");
                let addrs2 = addrs.replace(/.*--/, "");
                let addr = addrs2.replace(/ /g, "%20");
                full = data1.replace(/.*:/, "");
                full2 = full.replace(/ --.*/, "");
                let url = "http://cs310.ugrad.cs.ubc.ca:11316/api/v1/project_e6y0b_s5c1b/" + addr;
                chunks.forEach((data2: any) => {
                    let correctCode = data2[0].replace(/:.*/, "");
                    // console.log(correctCode);

                    if (correctCode === short) {
                        // console.log(data2[0] + addrs2);
                        let regex = /.*\//;
                        let roomname = data2[0].replace(regex, "");
                        let roomname2 = roomname.replace(/.*-/, "");
                        let roomname3 = short + "_" + roomname2;
                        let seats = data2[1].replace(/.*:/, "");
                        let seats2 = Number(seats);
                        let type = data2[3].replace(/.*:/, "");
                        let furn = data2[2].replace(/.*:/, "");
                        let href = data2[0].replace(/.*:/, "");
                        fx.push(that.getGeo(url, full2, short, roomname2, roomname3, addrs2, seats2, type, furn, href));
                    }
                });
            });
            let fxarray: any[] = [];
            Promise.all(fx).then((j: any) => {
                fxarray.push(j);
                return resolve(fxarray);
                // console.log(that.roomsMap);
            }).catch((err) => {
                // console.log("caught it " +  err); // some coding error in handling happened
            });
            // return resolve("ee");
        });
    }

    // GET DATA FROM THE ROOMS FROM THE LINKS
    private getDataNames(data: any, code: string) {
        // return new Promise<any[]>((resolve, reject) => {
        let promises: any[] = [];
        let document: any = parse5.parse(data);
        let dataNodes: any[] = document.childNodes;
        dataNodes.forEach((childnode: any) => {
            if (childnode.tagName === "html") {
                this.getDataName(childnode, code);
                this.getDataAddress(childnode, code);
            }
            // });
        });
    }

    // GET DATA NAME
    private getDataName(data: any, code: string) {
        // return new Promise<any[]>((resolve, reject) => {
        let dataNodes: any[] = data.childNodes;
        dataNodes.forEach((childnode: any) => {
            if (childnode.tagName === null || childnode.tagName === undefined) {
                // fuck
            } else if (childnode.tagName !== "h2") {
                this.getDataName(childnode, code);
            } else {

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

                this.nameArray1.push(code + ":" + childnode.value);
            }
        });
        // });
    }

    // GET DATA ADDRESS
    private getDataAddress(data: any, code: string) {
        let dataNodes: any[] = data.childNodes;
        let classVal = "building-info";
        dataNodes.forEach((childnode: any) => {
            if (childnode.tagName === null || childnode.tagName === undefined) {
                // fuck
            } else if (childnode.tagName !== "div") {
                this.getDataAddress(childnode, code);
            } else {
                if (typeof childnode.attrs !== "undefined") {
                    childnode.attrs.forEach((at: any) => {
                        if (childnode.attrs.length >= 1) {
                            if (at.value === classVal) {
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
                    this.addressArray1.push(code + "--" + childnode.value);
                }
            }
        });
    }

    // GET DATA FROM ROOM NUMBER
    private getDataRoomNumInfo(data: any, code: string) {
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

    private getAll(data: any, code: string) {
        let dataNodes: any[] = data.childNodes;
        let res: any[] = [];
        dataNodes.forEach((childnode: any) => {
            if (childnode.tagName === null || childnode.tagName === undefined || childnode.length === 0) {
                // fuck
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
        let c7 = "odd views-row-first views-row-last";
        let res: any[] = [];
        prom.forEach((datatr: any) => {
            if (typeof datatr.attrs !== "undefined") {
                datatr.attrs.forEach((at: any) => {
                    if (datatr.attrs.length >= 1) {
                        let x = at.value;

                        if (x === c1 || x === c2 || x === c3 || x === c4 || x === c5 || x === c6 || x === c7) {

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

            x.forEach((dats: any) => {
                if (dats.tagName === "td") {

                    if (typeof dats.attrs !== "undefined") {
                        dats.attrs.forEach((atr: any) => {
                            if (atr.value === l1 || atr.value === l2 || atr.value === l3 || atr.value === l4) {

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

                            res.push(code + "-" + tex2.value);
                        }
                    });
                    y.forEach((tex: any) => {
                        if (tex.name === "href") {

                            res.push(code + ":" + tex.value);
                        }
                    });
                } else {
                    let str = cnodes.value.replace(/\s/g, "");
                    let str2 = cnodes.value.trim();
                    res.push(code + ":" + str2);
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
                        // delete later
                    } else {
                        // delete after
                    }
                });

                this.mixArray1.push(res);
            }
        }
    }

    private addRooms(data: any, code: any, shortCodeArray: any, id: any) {
        let that = this;
        that.getDataNames(data, code);
        that.getDataRoomNumInfo(data, code);
    }

    public addDataset(id: string, content: string, kind: InsightDatasetKind): Promise<string[]> {
        let that = this;
        let promises: any[] = [];
        return new Promise<string[]>(function (resolve, reject) {

            if (typeof id !== "string" || typeof content !== "string" || kind === undefined || kind === null) {
                reject(new InsightError("Invalid params"));
            }
            if (that.coursesMap.has(id) || that.roomsMap.has(id)) {
                return reject(new InsightError("Id is already added"));
            }
            if (!id || id.length === 0 || id === "") {
                return reject(new InsightError("Invalid Id"));
            }
            // add room -----------------------
            if (kind === InsightDatasetKind.Rooms) {
                that.roomsMap.set(id, []);
                JSZip.loadAsync(content, {base64: true}).then(async (zipRooms: JSZip) => {
                    let promiseRooms = await zipRooms.file("index.htm").async("text");
                    that.addRoomLinks(promiseRooms, id);
                    let resultSaver: any[] = [];
                    let shortCodeArray: any[] = [];
                    let roomCodes = Array.from(that.linksMap.keys());
                    roomCodes.forEach((code: any) => {
                        shortCodeArray.push(code);
                        let value = that.linksMap.get(code);
                        let subs = value[0].slice(2, 47);
                        resultSaver.push(zipRooms.file(subs).async("text").then((datax: any) => {
                            that.addRooms(datax, code, shortCodeArray, id);
                            // that.placeData(id, shortCodeArray);
                        }).catch((e) => {
                            return reject(new InsightError("Error in adding roomsdataset " + e));
                        }));
                    });
                    Promise.all(resultSaver).then( async (t: any) => {
                        let res = await that.placeData(id, shortCodeArray);
                        res.forEach((x: any) => {
                            x.forEach((y: any) => {
                                that.roomsMap.get(id).push(y);
                            });
                        });
                        // that.roomsMap.get(id).push(x);
                        let toSaveOnDisk: object[] = that.roomsMap.get(id);
                        // console.log(toSaveOnDisk);
                        fs.writeFile("data/" + id + ".json", JSON.stringify(toSaveOnDisk), function (e) {
                            return reject(new InsightError("Error Saving Files " + e));
                        });
                        let result: any[] = [];
                        that.roomsMap.forEach(function (value, key) {
                            // console.log(key);
                            result.push(key);
                        });
                        let poth = process.cwd() + "/data";
                        let k = fs.readdirSync(poth);
                        k.forEach((rex) => {
                            if (rex.match(/.*json/) && !rex.match(id + ".json")) {
                                let dat = rex.replace(/\.json/, "");
                                result.push(dat);
                            }
                        });
                        return resolve(result);
                    }).catch((e) => {
                        // console.log("caught  " + e);
                    });
                    // }));
                }).catch((e) => {
                    that.roomsMap.delete(id);
                    return reject(new InsightError("Error decoding contents of rooms: Invalid Zip " + e));
                });
            } else if (kind === InsightDatasetKind.Courses) {
                // ---------------------------------
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
                                        let sec: string = section.Section;

                                        // let validCourse: CourseSaver = new CourseSaver(dept, id,
                                        //     avg, instructor, title, pass, fail, audit, uuid, year);
                                        if (sec === "overall") {
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
                                            validCourse.year = 1900;
                                            that.coursesMap.get(id).push(validCourse);
                                            that.mydatasets.set(id, validCourse);
                                        } else {
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
                                            that.mydatasets.set(id, validCourse);
                                        }
                                    });
                                } catch (e) {
                                    // not in JSON format or some fields of different type/missing-> skip this course
                                }
                            }).catch((e) => {
                                return reject(new InsightError("Error in adding coursesdataset " + e));
                            }));
                        }));
                    } else {
                        that.coursesMap.delete(id);
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
                        let poth = process.cwd() + "/data";
                        let x = fs.readdirSync(poth);
                        x.forEach((res) => {
                            if (res.match(/.*json/) && !res.match(id + ".json")) {
                                let dat = res.replace(/\.json/, "");
                                result.push(dat);
                            }
                        });
                        // console.log(result);
                        return resolve(result);
                        // }
                    });
                }).catch((e) => {
                    that.coursesMap.delete(id);
                    return reject(new InsightError("Error decoding contents of courses: Invalid Zip " + e));
                });
            }
            // rooms map is added here ------------------------
        });
    }
    public removeDataset(id: string): Promise<string> {
        let that = this;
        // console.log(process.cwd());
        return new Promise<string>(function (resolve, reject) {
            let x =  process.cwd() + "/data/" + id + ".json";
            if (id === "") {
                return reject(new InsightError("Invalid ID"));
                // reject({code: 400, body: {error: "Invalid ID"}});
            } else if (id === null || !id) {
                return reject(new InsightError("Invalid ID"));
                // reject({code: 400, body: {error: "Invalid ID"}});
            } else if (id === "rooms") {
                if ((!that.roomsMap.has(id))) {
                    return reject(new NotFoundError("NotFound: Id not in Map"));
                    // reject({code: 400, body: {error: "ID not in rooms Map"}});
                }
            } else if (id === "courses") {
                if (!that.coursesMap.has(id)) {
                    return reject(new NotFoundError("NotFound: Id not in Map"));
                    // reject({code: 400, body: {error: "ID not in courses Map"}});
                }
            }
            if (that.roomsMap.has(id)) {
                fs.unlink(x, function (err) {
                    if (err) {
                            return reject(new NotFoundError("NotFound: Dataset has not yet been loaded"));
                        } else {
                        that.roomsMap.delete(id);
                        // console.log("made it here");
                        return resolve(id);
                    }
                });
            } else if (that.coursesMap.has(id)) {
                fs.unlink(x, function (err) {
                    if (err) {
                        return reject(new NotFoundError("NotFound: Dataset has not yet been loaded"));
                    } else {
                        that.coursesMap.delete(id);
                        // console.log("made it here");
                        return resolve(id);
                    }
                });
            }
        });
    }
    public performQuery(query: any): Promise<any[]> {
        let that = this;
        return new Promise<any[]>(function (resolve, reject) {
            try {
                // let cwd = process.cwd();
                // console.log(cwd);
                let filter: InsightFilter = query.WHERE;
                let options = query.OPTIONS;
                let order = options.ORDER;
                let columns = options.COLUMNS;
                let transformations = query.TRANSFORMATIONS;
                let id: string = columns[0].split("_")[0];
                let dataset;
                // check map is here
                // console.log(that.roomsMap);

                // if (that.coursesMap.get(id)) {
                //     dataset = that.coursesMap.get(id);
                // } else if (that.roomsMap.get(id)) {
                //     dataset = that.roomsMap.get(id);
                //    // console.log(dataset);
                // } else {
                //     throw new Error("Can't find dataset with id: " + id);
                // }
                // if (that.coursesMap.get(id) === undefined) {
                //     reject(new InsightError("noot"));
                // }

                // let dataset = id === "courses" ? that.coursesMap.get(id) : that.roomsMap.get(id);

                let result: any[];
                if (id === COURSES) {
                    if (Object.keys(filter).length === 0) {
                        // console.log(that.coursesMap);
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
                        dataset = thisResult; // 25 results
                    }
                } else if (id === ROOMS) {
                    // console.log(that.roomsMap.get(id));
                    if (Object.keys(filter).length === 0) {
                        result = that.roomsMap.get(id);

                        if (result.length > 5000) {
                            throw new InsightError("Too many sections in result");
                        }
                    } else {
                        let thisResult: any[] = [];
                        for (let room of that.roomsMap.get(id)) {
                            // console.log(room);
                            // check if you can apply filter to the key
                            if (InsightFacade.isSectionValid(filter, room, id)) {
                                thisResult.push(room);
                            }
                        }
                        // result over 5000 to add
                        if (thisResult.length > 5000) {
                            throw new InsightError("Result exceeds 5000 limit");
                        }
                        dataset = thisResult; // 25 results
                        // console.log(dataset);
                    }
                }
                for (let q of Object.keys(query)) {
                    if (q !== "WHERE") {
                        if (q !== "OPTIONS") {
                            if (q !== "TRANSFORMATIONS") {
                                throw new InsightError("Now this should work");
                            }
                        }
                    }
                }

                if (transformations !== undefined && transformations !== null) {
                    let keys: any[] = Object.keys(query);

                    if (!Object.keys(transformations).includes("GROUP", 0)) {
                        throw new InsightError("must have GROUP spelled");
                    }
                    if (!Object.keys(transformations).includes("APPLY")) {
                        throw new InsightError("must have APPLY spelled");
                    }
                    let transformedDataset = [];
                    let groups: Map<string, any> = new Map<string, any>();
                    if (transformations.GROUP === undefined || transformations.GROUP.length === 0) {
                        throw new InsightError("GROUP must be non-empty array");
                    }

                    if (id === COURSES) {
                        for (let values of dataset) {
                            let groupingVal: string = "";
                            for (let pair of transformations.GROUP) {
                                if (!query.OPTIONS.COLUMNS.includes(pair) &&
                                    !InsightFacade.validCourseKeyHelper(pair, id)) {
                                    throw new InsightError("invalid key in GROUP");
                                }
                                let key = pair.split("_")[1]; // key = title
                                let value: string = values[key] as string; // grab value from values[title]

                                if (values) {
                                    groupingVal += value; // "" + "Career Planning"
                                } else {
                                    throw new InsightError("the pair was not a valid key");
                                }
                            }

                            if (!groups.get(groupingVal)) {
                                groups.set(groupingVal, []); // each grouptitle has its array of grouped items
                            }
                            groups.get(groupingVal).push(values); // push sections that group to Career Planning
                        }
                        for (let group of groups.values()) { // group {title(1): ___} of title(n)
                            let entry: { [key: string]: any } = {}; // initialize {[key: stirng]: any}
                            for (let pair of transformations.GROUP) {
                                let key = pair.split("_")[1]; // title
                                entry[key] = group[0][key]; // [title: Career Planning]
                            }
                            let applyKeys: string[] = []; // array for keys to apply token on
                            for (let object of transformations.APPLY) { // object: overallAvg: {<key>:<token>}
                                let applyKey = Object.keys(object)[0]; // applyKey = overallAvg
                                if (!applyKey.includes("_")) {
                                    if (!applyKeys.includes(applyKey)) { // if token not in array
                                        applyKeys.push(applyKey);
                                    } else {
                                        throw new InsightError("Duplicated apply key not allowed");
                                    }
                                } else {
                                    throw new InsightError("applyKey cannot contain underscore");
                                }
                                let applyToken = Object.keys(object[applyKey])[0];
                                let pair = object[applyKey][applyToken]; // what token performs on: courses_avg
                                let key = pair.split("_")[1]; // avg
                                let setVal: any[] = [];
                                for (let element of group) {
                                    setVal.push(element[key]); // setVal has courses_avg for each section
                                }
                                let value: number;
                                if (applyToken === "MAX") {
                                    value = setVal[0];
                                    value = that.applyMaxHelper(value, setVal);
                                } else if (applyToken === "MIN") {
                                    value = setVal[0];
                                    value = that.applyMinHelper(value, setVal);
                                } else if (applyToken === "SUM") {
                                    let total = new Decimal(0);
                                    value = that.applySumHelper(total, setVal);
                                } else if (applyToken === "AVG") {
                                    let sum = new Decimal(0);
                                    value = that.applyAverageHelper(sum, setVal);
                                } else if (applyToken === "COUNT") {
                                    let unique = setVal.filter((values, index, self) => {
                                        return self.indexOf(values) === index;
                                    });
                                    value = unique.length;
                                } else {
                                    throw new InsightError("Token does not exist");
                                }
                                entry[applyKey] = value;
                            }
                            transformedDataset.push(entry);
                        }
                    } else if (id === ROOMS) {
                        for (let values of dataset) {
                            let groupingVal: string = "";
                            for (let pair of transformations.GROUP) {
                                if (!query.OPTIONS.COLUMNS.includes(pair) &&
                                    !InsightFacade.validCourseKeyHelper(pair, id)) {
                                    throw new InsightError("invalid key in GROUP");
                                }
                                // let key = pair.split("_")[1]; // key = title
                                let value: string = values[pair] as string; // grab value from values[title]

                                if (values) {
                                    groupingVal += value; // "" + "Career Planning"
                                } else {
                                    throw new InsightError("the pair was not a valid key");
                                }
                            }

                            if (!groups.get(groupingVal)) {
                                groups.set(groupingVal, []); // each grouptitle has its array of grouped items
                            }
                            groups.get(groupingVal).push(values); // push sections that group to Career Planning
                        }
                        for (let group of groups.values()) { // group {title(1): ___} of title(n)
                            let entry: { [key: string]: any } = {}; // initialize {[key: stirng]: any}
                            for (let pair of transformations.GROUP) {
                                // let key = pair.split("_")[1]; // title
                                entry[pair] = group[0][pair]; // [title: Career Planning]
                            }
                            let applyKeys: string[] = []; // array for keys to apply token on
                            for (let object of transformations.APPLY) { // object: overallAvg: {<key>:<token>}
                                let applyKey = Object.keys(object)[0]; // applyKey = overallAvg
                                if (!applyKey.includes("_")) {
                                    if (!applyKeys.includes(applyKey)) { // if token not in array
                                        applyKeys.push(applyKey);
                                    } else {
                                        throw new InsightError("Duplicated apply key not allowed");
                                    }
                                } else {
                                    throw new InsightError("applyKey cannot contain underscore");
                                }
                                let applyToken = Object.keys(object[applyKey])[0]; // Key of overallAvg[applyKey]
                                let pair = object[applyKey][applyToken]; // what token performs on: courses_avg
                                // let key = pair.split("_")[1]; // avg
                                let setVal: any[] = [];
                                for (let element of group) {
                                    setVal.push(element[pair]); // setVal has courses_avg for each section { 90, 80, 70
                                }
                                let value: number;
                                if (applyToken === "MAX") {
                                    value = setVal[0];
                                    value = that.applyMaxHelper(value, setVal);
                                } else if (applyToken === "MIN") {
                                    value = setVal[0];
                                    value = that.applyMinHelper(value, setVal);
                                } else if (applyToken === "SUM") {
                                    let total = new Decimal(0);
                                    value = that.applySumHelper(total, setVal);
                                } else if (applyToken === "AVG") {
                                    let sum = new Decimal(0);
                                    value = that.applyAverageHelper(sum, setVal);
                                } else if (applyToken === "COUNT") {
                                    let unique = setVal.filter((values, index, self) => {
                                        return self.indexOf(values) === index;
                                    });
                                    value = unique.length;
                                } else {
                                    throw new InsightError("Token does not exist");
                                }
                                entry[applyKey] = value;
                            }
                            transformedDataset.push(entry);
                        }
                    }

                    dataset = transformedDataset;
                }
                // keep only the desired columns in query
                if (columns && columns.length !== 0) {
                    let columnResult: object[] = [];
                    if (id === COURSES) {
                        dataset.forEach(function (section: any) {
                            let columnSection: any = {};
                            columns.forEach(function (key: any) {
                                let cols;
                                if (query.TRANSFORMATIONS !== undefined) {
                                    if (key.includes("_")) {
                                        if (query.TRANSFORMATIONS.GROUP !== undefined) {
                                            for (let b of query.TRANSFORMATIONS.GROUP) { // group has an array of keys
                                                if (b === key) {
                                                    cols = key.split("_")[1]; // cols = title
                                                    break;
                                                }
                                            }
                                            if (cols === undefined) {
                                                throw  new InsightError("Columns didnt map to any GROUP key");
                                            }
                                        }
                                    } else if (query.TRANSFORMATIONS.APPLY !== undefined) {
                                        for (let a of query.TRANSFORMATIONS.APPLY) { // apply has an array of keys
                                            if (Object.keys(a)[0] === key) {
                                                cols = key;
                                                break;
                                            }
                                        }
                                        if (cols !== key) {
                                            throw  new InsightError("Columns didnt map to any apply key");
                                        }
                                    } else {
                                        throw new InsightError("Columns key not in apply");
                                    }
                                } else {
                                    if (key.includes("_")) {
                                        cols = key.split("_")[1];
                                    } else {
                                        cols = key;
                                    }
                                }
                                columnSection[key] = section[cols];
                            });
                            columnResult.push(columnSection);
                        });
                        result = columnResult;
                    } else if (id === ROOMS) {
                        dataset.forEach(function (room: any) {
                            let columnRoom: any = {};
                            columns.forEach(function (key: any) {
                                let cols;
                                if (query.TRANSFORMATIONS !== undefined) {
                                    if (key.includes("_")) {
                                        if (query.TRANSFORMATIONS.GROUP !== undefined) {
                                            for (let b of query.TRANSFORMATIONS.GROUP) { // group has an array of keys
                                                if (b === key) {
                                                    // cols = key.split("_")[1]; // cols = title
                                                    cols = key;
                                                    break;
                                                }
                                            }
                                            if (cols === undefined) {
                                                throw  new InsightError("Columns didnt map to any GROUP key");
                                            }
                                        }
                                    } else if (query.TRANSFORMATIONS.APPLY !== undefined) {
                                        for (let a of query.TRANSFORMATIONS.APPLY) { // apply has an array of keys
                                            if (Object.keys(a)[0] === key) {
                                                cols = key;
                                                break;
                                            }
                                        }
                                        if (cols !== key) {
                                            throw  new InsightError("Columns didnt map to any apply key");
                                        }
                                    } else {
                                        throw new InsightError("Columns key not in apply");
                                    }
                                } else {
                                    if (key.includes("_")) {
                                        // cols = key.split("_")[1];
                                        cols = key;
                                    } else {
                                        throw new InsightError("Incorrect KEY format");
                                    }
                                }
                                columnRoom[key] = room[cols];
                            });
                            columnResult.push(columnRoom);
                        });
                        result = columnResult;
                    }
                }

                if (order !== undefined && order !== null) {
                    if (columns.includes(order)) {
                        result = that.Sorter(result, order, columns);
                    } else if (order.dir) {
                        if (order.keys !== undefined) {
                            for (let c of order.keys) {
                                if (!columns.includes(c)) {
                                    throw new InsightError("NOT IN HERE");
                                }
                            }
                            result = that.Sorter(result, order, columns);
                        } else {
                            throw new InsightError("SHIT");
                        }
                    } else {
                        throw new InsightError("goddam");
                    }
                }
                // resolve if no problems
                return resolve(result);
                // resolve({code: 200, body: {result}});
            } catch (e) {
                 return reject(new InsightError("Error in reading query " + e));
                // reject({code: 400, body: {error: "Error in reading query" + e}});
            }

        });
    }

    private applyAverageHelper(sum: any, setVal: any[]): number {
        for (let values of setVal) {
            sum = sum.add(new Decimal(values));
        }
        return Number((Number(sum) / setVal.length).toFixed(2));
    }

    private applySumHelper(total: any, setVal: any[]): number {
        for (let values of setVal) {
            total = total.add(new Decimal(values));
        }
        return Number(total.toFixed(2));
    }

    private applyMaxHelper(value: number, setVal: any[]): number {
        for (let values of setVal) {
            if (typeof values !== "number") {
                throw new InsightError("MAX uses nubmers only");
            } else {
                if (values > value) {
                    value = values;
                }
            }
        }
        return value;
    }

    private applyMinHelper(value: number, setVal: any[]): number {
        for (let values of setVal) {
            if (typeof values !== "number") {
                throw new InsightError("MAX uses nubmers only");
            } else {
                if (values < value) {
                    value = values;
                }
            }
        }
        return value;
    }

    private Sorter(result: any[], order: any, columns: string[]): any[] {
        let key = order as string;
        let sorted: any;
        if (typeof order === "string") {
            // columns needs sorting key
            if (columns.includes(key)) {
                result.sort((left, right): any => {
                    if (left[key] > right[key]) {
                        return 1;
                    } else if (left[key] < right[key]) {
                        return -1;
                    }
                });
            } else {
                throw new Error("Order key MUST BE IN COLUMNS");
            }
            sorted = result;
        } else {
            let keyOrder = order.keys; // array of keys in order
            let cPointer: number;
            // need nested functio (closure) to access all variables and functions
            // defined inside outer function
            function Larger(left: any, right: any, keys: string): number {
                if (!columns.includes(keys)) {
                    throw new InsightError("Order key MUST BE IN COLUMNS x 2");
                }
                if (left[keys] > right[keys]) {
                    return 1;
                } else if (left[keys] < right [keys]) {
                    return -1;
                } else {
                    cPointer += 1;
                    if (cPointer < keyOrder.length) {
                        return Larger(left, right, keyOrder[cPointer]);
                    } else {
                        return 0;
                    }
                }
            }

            result.sort((left, right) => {
                cPointer = 0;
                if (order.dir === undefined || order.dir === null) {
                    throw new Error("disgusting");
                } else if (order.dir === "DOWN") {
                    return Larger(right, left, keyOrder[cPointer]);
                } else if (order.dir === "UP") {
                    return Larger(left, right, keyOrder[cPointer]);
                } else {
                    throw new Error("Not even a value");
                }
            });
            sorted = result;
        }
        return sorted;
    }

    private static validRoomsKeyHelper(key: string, id: string): boolean {
        // check if the key being passed is a valid one
        if (id === COURSES) {
            throw new InsightError("cannot query courses as rooms");
        }
        switch (key) {
            case id + "_fullname":
                return true;
            case id + "_shortname":
                return true;
            case id + "_number":
                return true;
            case id + "_name":
                return true;
            case id + "_address":
                return true;
            case id + "_lat":
                return true;
            case id + "_lon":
                return true;
            case id + "_seats":
                return true;
            case id + "_type":
                return true;
            case id + "_furniture":
                return true;
            case id + "_href":
                return true;
            default:
                return false;
        }
    }

    private static validCourseKeyHelper(key: string, id: string): boolean {
        // check if the key being passed is a valid one
        if (id === ROOMS) {
            throw new InsightError("cannot query rooms as courses");
        }
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
            if (filter.AND.length === 0) {

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
            let body = Object.values(filter)[0];
            // MCOMPARATOR must be a number
            // check for valid value
            if (typeof Object.values(body)[0] !== "number") {
                throw new InsightError("Invalid value");
            }
            if (id === "courses") {
                if (!this.validCourseKeyHelper(Object.keys(body)[0], id)) {
                    throw new InsightError("Invalid Course key");
                }
                switch (Object.keys(body)[0]) {
                    case id + "_dept":
                        throw new InsightError("Not valid key");
                    case id + "_id":
                        throw new InsightError("Not valid key");
                    case id + "_instructor":
                        throw new InsightError("Not valid key");
                    case id + "_title":
                        throw new InsightError("Not valid key");
                    case id + "_uuid":
                        throw new InsightError("Not valid key");
                    default:
                        break;
                }
            } else if (id === "rooms") {
                if (!this.validRoomsKeyHelper(Object.keys(body)[0], id)) {
                    throw new InsightError("Invalid Rooms key");
                }
                // console.log(Object.keys(body)[2]);
                switch (Object.keys(body)[0]) {
                    case id + "_fullname":
                        throw new InsightError("Not valid key");
                    case id + "_shortname":
                        throw new InsightError("Not valid key");
                    case id + "_number":
                        throw new InsightError("Not valid key");
                    case id + "_address":
                        throw new InsightError("Not valid key");
                    case id + "_type":
                        throw new InsightError("Not valid key");
                    case id + "_furniture":
                        throw new InsightError("Not valid key");
                    case id + "_href":
                        throw new InsightError("Not valid key");
                    default:
                        break;
                }
                if (filter.hasOwnProperty("GT")) {
                    if (section[Object.keys(filter.GT)[0]] > Object.values(filter.GT)[0]) { // section[rooms_lat]
                        return true;
                    } else {
                        return false;
                    }
                } else if (filter.hasOwnProperty("LT")) {
                    if (section[Object.keys(filter.LT)[0]] < Object.values(filter.LT)[0]) {
                        return true;
                    } else {
                        return false;
                    }
                } else {

                    if (section[Object.keys(filter.EQ)[0]] === Object.values(filter.EQ)[0]) {

                        return true;
                    } else {
                        return false;
                    }
                }
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

                if (section[yup] === Object.values(filter.EQ)[0]) {

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

            let key: any = Object.keys(filter.IS)[0]; // courses_id
            // check if key is not invalid

            let value: any = Object.values(filter.IS)[0]; // courses_avg: VALUE
            // check if value is of right type
            if (typeof value !== "string") {
                throw new InsightError("Invalid type");
            }
            if (id === "courses") {
                if (!this.validCourseKeyHelper(key, id)) {
                    throw new InsightError("Invalid Course key");
                }
                switch (key) {
                    case id + "_avg":
                        throw new InsightError("Not valid key");
                    case id + "_pass":
                        throw new InsightError("Not valid key");
                    case id + "_fail":
                        throw new InsightError("Not valid key");
                    case id + "_year":
                        throw new InsightError("Not valid key");
                    case id + "_audit":
                        throw new InsightError("Not valid key");
                    default:
                        break;
                }
            } else if (id === "rooms") {
                if (!this.validRoomsKeyHelper(key, id)) {
                    throw new InsightError("Invalid Rooms key");
                }
                switch (key) {
                    case id + "_lat":
                        throw new InsightError("Not valid key");
                    case id + "_lon":
                        throw new InsightError("Not valid key");
                    case id + "_seats":
                        throw new InsightError("Not valid key");
                    default:
                        break;
                }
                let actualRoom: string = section[key]; // section[rooms_lat]
                // console.log(actualRoom);
                // check each wildcard case
                if (value.includes("*")) {
                    let valueArray: string[] = value.split("*");
                    if (valueArray.length === 2) {
                        if (valueArray[0] === "") {
                            if (valueArray.length > 2) {
                                throw new InsightError("Asterisks cannot be in the middle");
                            }
                            if (value.startsWith("*")) {
                                return actualRoom.endsWith(value.substring(1));
                            }
                        }
                        if (valueArray[1] === "") {
                            if (valueArray.length > 2) {
                                throw new InsightError("Asterisks cannot be ");
                            }
                            if (value.endsWith("*")) {
                                return actualRoom.startsWith(value.substring(0, value.length - 1));
                            }
                        } else {
                            throw new InsightError("Asterisk Error Occured");
                        }
                    }
                    if (valueArray.length === 3 && valueArray[0] === "" && valueArray[2] === "") {
                        if (value.startsWith("*") && value.endsWith("*")) {
                            return actualRoom.includes(value.substring(1, value.length - 1));
                        }
                    } else {
                        // h**lo or h*llo === error
                        throw new InsightError("Asteriks cannot be in the middle");
                    }
                }
                return value === actualRoom;
            }

            let actualRes: string = section[key.substring(key.indexOf("_") + 1)]; // section[id]
            // check each wildcard case
            if (value.includes("*")) {
                let valueArray: string[] = value.split("*");
                if (valueArray.length === 2) {
                    if (valueArray[0] === "") {
                        if (valueArray.length > 2) {
                            throw new InsightError("Asterisks cannot be in the middle");
                        }
                        if (value.startsWith("*")) {
                            return actualRes.endsWith(value.substring(1));
                        }
                    }
                    if (valueArray[1] === "") {
                        if (valueArray.length > 2) {
                            throw new InsightError("Asterisks cannot be ");
                        }
                        if (value.endsWith("*")) {
                            return actualRes.startsWith(value.substring(0, value.length - 1));
                        }
                    } else {
                        throw new InsightError("Asterisk Error Occured");
                    }
                }
                if (valueArray.length === 3 && valueArray[0] === "" && valueArray[2] === "") {
                    if (value.startsWith("*") && value.endsWith("*")) {
                        return actualRes.includes(value.substring(1, value.length - 1));
                    }
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
        return new Promise<InsightDataset[]>( function (resolve, reject) {
                let result2: InsightDataset[] = [];
                for (let id of that.coursesMap.keys()) {
                    let crows: number = that.coursesMap.get(id).length;
                    result2.push({id, kind: InsightDatasetKind.Courses, numRows: crows});
                }
                for (let id of that.roomsMap.keys()) {
                    let crows: number = that.roomsMap.get(id).length;
                    result2.push({id, kind: InsightDatasetKind.Rooms, numRows: crows});
                }
                // resolve({code: 200, body: result2});
                return resolve(result2);
        });
    }
}
