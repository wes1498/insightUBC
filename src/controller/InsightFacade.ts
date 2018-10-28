import Log from "../Util";
import {IInsightFacade, InsightDataset, InsightDatasetKind, CourseSaver, InsightFilter, InsightCourse} from "./IInsightFacade";
import {InsightError, NotFoundError} from "./IInsightFacade";
import * as JSZip from "jszip";
import {getRelativePath} from "tslint/lib/configuration";
import {relative} from "path";
import {JSZipObject} from "jszip";
import * as fs from "fs";
import * as Path from "path";

/**
 * This is the main programmatic entry point for the project.
 * Method documentation is in IInsightFacade
 *
 */

export default class InsightFacade implements IInsightFacade {
    private coursesMap: Map<string, any>;
    private result: any[];
    constructor() {
        Log.trace("InsightFacadeImpl::init()");
        this.coursesMap = new Map<string, any>();
    }
    public addDataset(id: string, content: string, kind: InsightDatasetKind): Promise<string[]> {
        let that = this;
        return new Promise<string[]>(function (resolve, reject) {
            // let loadedyet: boolean = false;
            // if (that.coursesMap.has(id) || fs.existsSync(`${id}.json`)) {
            //     loadedyet = true;
            // } else if (!id || id.length === 0 || id === "") {
            //     return reject(new InsightError("Invalid Id"));
            // }
            if (that.coursesMap.has(id)) {
                return reject(new InsightError("Id is already added"));
            } else if (!id || id.length === 0 || id === "") {
                return reject(new InsightError("Invalid Id"));
            }
            that.coursesMap.set(id, []);
            JSZip.loadAsync(content, {base64: true}).then(function (unzipped: JSZip) {
                let filesPromise: any[] = [];
                if (unzipped.length > 1  || unzipped.files.hasOwnProperty("courses/")) {
                unzipped.forEach((function (relativePath, fileObject: JSZipObject) {
                    // if (fileObject.dir) {
                    //     return;
                    // }
                    let promise = fileObject.async("text").then(function (course: any) {
                        try {
                            let parsedInfo = JSON.parse(course);
                            if ((parsedInfo.result.length === 0)) {
                                return reject(new NotFoundError("result is 0"));
                            }
                            parsedInfo.result.forEach(function (element: any) {
                                let validCourse: any = that.CoursesHelper(element);
                                that.coursesMap.get(id).push(validCourse);
                            });
                        } catch (e) {
                            console.log(`Failed to parse file ${relativePath}. Skipping file.`);
                            // not in JSON format or some fields of different type/missing-> skip this course
                        }

                    }).catch(function (err: any) {
                        return reject(new NotFoundError("Async Error " + err));
                    });
                    filesPromise.push(promise);
                    // filesPromise.push(fileObject.async("text").then(function (data: string) {
                    //     try {
                    //         let parsedInfo = JSON.parse(data);
                    //         if ((parsedInfo.result.length === 0)) {
                    //             throw new Error("No sections.");
                    //         }
                    //         parsedInfo.result.forEach(function (section: any) {
                    //             let validCourse: any = that.CoursesHelper(section);
                    //             that.coursesMap.get(id).push(validCourse);
                    //         });
                    //     } catch (e) {
                    //         // not in JSON format or some fields of different type/missing-> skip this course
                    //     }
                    // }).catch(function (e) {
                    //     return reject(new InsightError("Error processing encoded course data " + e));
                    // }));
                        // }
                }));
                } else {
                    return reject(new InsightError("Desired folder for the dataset kind does not exist"));
                }
                Promise.all(filesPromise).then(function () {
                    let toSaveOnDisk: object[] = that.coursesMap.get(id);
                    if (toSaveOnDisk.length === 0) {
                        that.coursesMap.delete(id);
                        return reject(new InsightError("No sections were added"));
                    } else {
                        fs.writeFile("data/" + id, JSON.stringify(toSaveOnDisk), function (e) {
                            return reject(new InsightError("Error Saving Files " + e));
                        });

                        let result: string[] = [];
                        that.coursesMap.forEach(function (value, key) {
                            result.push(key);
                        });
                        return resolve(result);
                    }
                });
            }).catch(function (e) {
                return reject(new InsightError("Error decoding contents: Invalid Zip " + e));
            });
        });
    }
    private CoursesHelper(section: any): CourseSaver {
        let validCourse: CourseSaver = new CourseSaver(section.Subject, section.Course, section.Avg, section.Professor, section.Title, section.Pass, section.Fail, section.Audit, section.id.toString(), Number(section.Year));
        return validCourse;
    }

    // private addPromises (zip: JSZip, id: string): Promise<string[]> {
    //       return new Promise<string[]>((resolve, reject) => {
    //           let store: any[] = [];
    //           zip.forEach(((relativePath, file: JSZipObject ) => {
    //              // let store: any[] = [];
    //               store.push(file.async("text").then((data: string) => {
    //                   this.addCourse(data, id).then((data2: any) => {
    //                       // promises.push(data2);
    //                        // console.log("yeeeeeeeeeee" + JSON.stringify(data2));
    //                        store.push(data2);
    //                        // console.log(id);
    //                       // console.log(store);
    //                       // return resolve(store);
    //                       // return data2;
    //                   });
    //                   // console.log( "from add " + store);
    //                   // return resolve(store);
    //                   }
    //               ));
    //               // return resolve(store);
    //           }));
    //           return resolve(store);
    //       });
    // }
//     public addDataset2(id: string, content: string, kind: InsightDatasetKind): Promise<string[]> {
//         return new Promise<string[]>((resolve, reject) => {
//             let promises: any[] = [];
//             // Unzipping returns a JSzip
//             // this.coursesMap.set(id, []);
//             JSZip.loadAsync(content, {base64: true}).then((zip: JSZip) => {
//                 // console.log(zip);
//             /*    this.addPromises(zip, id).then((data: string[]) => {
//                     // console.log(data);
//                     promises = data;
//                     // console.log(this.coursesMap.get(id));
//                 });*/
//                 // console.log(this.coursesMap.get(id));
//                 // console.log(this.coursesMap.entries());
//                 zip.forEach(((relativePath, file: JSZipObject) => {
//                 promises.push(file.async("text").then((data: string) => {
//                     this.addCourse(data, id).then((data2: any) => {
//                         console.log(promises);
//                         return data2;
//                     });
//                     // console.log(promises);
//                     // console.log(this.coursesMap); // save the course info a
//                 }).catch((e) => {
//                     return reject(new InsightError("1 " + e));
//                 }));
//             }));
//                 Promise.all(promises).then((result) => {
//                     console.log(promises);
//                     let arr: string[] = this.coursesMap.get(id);
//                     // console.log(arr);
//                     console.log(this.coursesMap.get(id));
//                     let dat = Object.assign({}, arr);
//                     fs.writeFile("msg" + id + ".txt", JSON.stringify(arr), (e) => {
//                         if (e) {
//                             console.log(e + " error");
//                         } else {
//                             return Promise.resolve(arr);
//                         }
//                     });
//                     return result;
//                 });
//             }).catch((e) => {
//                 return reject(new InsightError("zip " + e));
//             });
//         });
// }
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
    // private addCourse(data: string, datasetId: string): Promise<any[]> {
    //     return new Promise<any[]>((resolve, reject) => {
    //         // console.log(data);
    //         // let dat = Object.assign({}, data);
    //         // console.log(dat);
    //         try {
    //         let parsedData = JSON.parse(data);
    //         let filterPromises: any[] = [];
    //         if (parsedData.result.length === 0) {
    //             return reject("0 section.");
    //         }
    //         this.coursesMap.set(datasetId, []);
    //         let dataObj: object[] = parsedData.result;
    //         dataObj.forEach((section: any) => {
    //             this.filters([section], datasetId).then((stuff3: any) => {
    //                 let dataObjParse: any[] = stuff3;
    //                 // this.coursesMap.set(datasetId, []);
    //                 this.coursesMap.get(datasetId).push(dataObjParse);
    //                 // console.log(this.coursesMap.get(datasetId));
    //                 // console.log(datasetId);
    //                 return resolve(dataObjParse);
    //             });
    //         });
    //     } catch (e) {
    //             console.log(e);
    //             return reject("Error");
    //         }
    // });
    // }
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
    // private  filters (someone: object[], datasetId: string): Promise<InsightCourse> {
    //     return new Promise<InsightCourse>((resolve, reject) => {
    //        /* if (someone === undefined) {
    //             return reject(new InsightError("it is undefined"));
    //         }*/
    //         someone.map((sect: any) => {
    //         let subject: string = sect.Subject as string;
    //         let year: number = sect.Year as number;
    //         let professor: string = sect.Professor as string;
    //         let id: string = sect.Course as string;
    //         let avg: number = sect.Avg as number;
    //         let title: string = sect.Title as string;
    //         let pass: string = sect.Pass as string;
    //         let fail: string = sect.Fail as string;
    //         let uuid: string = sect.id.toString() as string;
    //         let audit: number = sect.Audit as number;
    //
    //         let filteredCourse: InsightCourse = {
    //              courses_dept: subject,
    //              courses_id: id,
    //              courses_avg: avg,
    //              courses_instructor: professor,
    //              courses_title: title,
    //              courses_pass: pass,
    //              courses_fail: fail,
    //              courses_uuid: uuid,
    //              courses_audit: audit,
    //              courses_year: year,
    //          };
    //         // this.coursesMap.get(datasetId).push(filteredCourse);
    //         return resolve(filteredCourse);
    //  });
    // });
    // }
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
    // public removeDataset(id: string): Promise<string> {
    //     let that = this;
    //     return new Promise<string>(function (resolve, reject) {
    //         if (id === "") {
    //             return reject(new NotFoundError("Invalid ID"));
    //         } else if (id === null || !id) {
    //             return reject(new InsightError("Invalid ID"));
    //         }
    //         let pathName: string = `${id}.json`;
    //         // So if there is not an existing path name of that type in cache reject
    //         if (!fs.existsSync(pathName)) {
    //             return reject (new NotFoundError("Not Found in path"));
    //         } else if (fs.existsSync(pathName)) {
    //             // delete the file name from the system and its corresponding space used
    //             fs.unlinkSync(pathName);
    //             if (that.coursesMap.has(id)) {
    //                 // if the map contains it, then delete it from there
    //                 that.coursesMap.delete(id);
    //             } else {
    //                 return reject(new NotFoundError("Id Not in Map"));
    //             }
    //             return resolve(id);
    //         }
    //     });
    // }

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
                    } else {
                        if (files.includes(id)) {
                            const pathy = Path.join(InsightDatasetKind.Courses, id);
                            fs.unlink(pathy, function (e1) {
                                if (e1) {
                                    return reject(new InsightError("Dataset no unlink " + e1));
                                }
                            });
                        } else {
                            return reject(new NotFoundError("File does not include Id"));
                        }
                    }
                });
            } else if (!that.coursesMap.has(id)) {
                return reject(new NotFoundError("Not found"));
            }
            that.coursesMap.delete(id);
            return resolve (id);
        });
    }

    public performQuery(query: any): Promise<any[]> {
        let that = this;
        return new Promise<any[]>(function (resolve, reject) {
            try {
                // let filter: InsightFilter = query.WHERE;
                // let options = query.OPTIONS;
                // let order = options.ORDER;
                // let columns = options.COLUMNS;
                // let id: string = query.OPTIONS[0].split("_")[0];
                let id: string = /(.*)_.*/gi.exec(query["OPTIONS"]["COLUMNS"][0])[1];

                // let dataset = id;
                let result: any[];
                // Empty WHERE protection
                if (Object.keys(query.WHERE).length === 0) {
                    result = that.coursesMap.get(id);
                    if (result.length > 5000) {
                        throw new InsightError("Too many sections in result"); }
                } else {
                    for (let section of that.coursesMap.get(id)) {
                        if (InsightFacade.isSectionValid(query.WHERE, section)) {
                            that.result.push(section);
                        }
                        if (that.result.length > 5000) {
                            throw new InsightError("Result exceeds 5000 limit");
                        }
                    }
                }

                // keep only the desired columns in query
                if (query.OPTIONS.COLUMNS && query.OPTIONS.COLUMNS.length !== 0) {
                    let columnResult: object[] = [];
                    let filteredSections: any = result;
                    filteredSections.forEach( function (section: any) {
                        let columnSection: any = {};
                        query.OPTIONS.COLUMNS.forEach( function (key: any) {
                            if (InsightFacade.validKeyHelper(key)) {
                                columnSection[key] = section[key];
                            }
                        });
                        columnResult.push(columnSection);
                    });
                    result = columnResult;
                    // result = this.desiredColumnsHelper(result, columns);
                }
                // Sort the result if order is included
                if (query.OPTIONS.ORDER !== undefined || query.OPTIONS.ORDER !== null) {
                    if (query.OPTIONS.COLUMNS.includes(query.OPTIONS.ORDER)) {
                        result = result.sort( function (a, b) {
                            let x = a[query.OPTIONS.ORDER];
                            let y = b[query.OPTIONS.ORDER];
                            if (x === y) {
                                return 0;
                            } else if (x > y ) {
                                return 1;
                            } else {
                                return -1;
                            }
                        });
                        // this.rowOrderHelper(result, order);
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

    // For every course section in Query, check if filter applies a
    private static validKeyHelper(key: string): boolean {
        // check if the key being passed is a valid one
        switch (key) {
            case "courses_dept":
                return true;
            case "courses_id":
                return true;
            case "courses_instructor":
                return true;
            case "cpurses_title":
                return true;
            case "courses_uuid":
                return true;
            case "courses_avg":
                return true;
            case "courses_pass":
                return true;
            case "courses_fail":
                return true;
            case "courses_audit":
                return true;
            case "courses_year":
                return true;
            default:
                return false;
        }
    }

    private static handleSComparisonHelper(is: object, section: { [key: string]: string }): boolean {

        let key: any = Object.keys(is)[0];
        // check if key is not invalid
        if (!this.validKeyHelper(key)) {
            throw new InsightError("Invalid key");
        }
        let value: any = Object.values(is)[0];
        // check if value is of right type
        if (typeof value !== "string") {
            throw new InsightError("Invalid type");
        }
        if (key === "courses_avg") {
            throw new InsightError("Not valid key");
        } else if (key === "courses_pass") {
            throw new InsightError("Not valid key");
        } else if (key === "courses_fail") {
            throw new InsightError("Not valid key");
        } else if (key === "courses_year") {
            throw new InsightError("Not valid key");
        } else if (key === "courses_audit") {
            throw new InsightError("Not valid key");
        }
        let actualRes: string = section[key];
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
    }

    // Check if filter applies to given section
    private static isSectionValid(filter: InsightFilter, section: any): boolean {
        if (filter.hasOwnProperty("AND") || filter.hasOwnProperty("OR")) {
            if (filter.AND) {
                if (filter.AND.length === 0) {
                    throw new InsightError("Not enough conditions for AND");
                }
                for (let inside of filter.AND) {
                    if (!this.isSectionValid(inside, section)) {
                        return false;
                    }
                }
                return true;
            } else {
                if (filter.AND.length === 0) {
                    throw new InsightError("Not enough conditions for OR");
                }
                for (let inside of filter.OR) {
                    if (this.isSectionValid(inside, section)) {
                        return true;
                    }
                }
                return false;
            }
        } else if (filter.GT.hasOwnProperty("GT") || filter.LT.hasOwnProperty("LT") || filter.EQ.hasOwnProperty("EQ")) {
            if (typeof Object.values(Object.values(filter)[0]) !== "number") {
                throw new InsightError("Invalid value");
            } else {
                if (!this.validKeyHelper(Object.keys(filter)[0])) {
                    throw new InsightError("Invalid key");
                } else if (this.validKeyHelper(Object.keys(filter)[0])) {
                    if (filter.hasOwnProperty("GT")) {
                        return section[Object.keys(filter.GT)[0]] > Object.values(filter.GT)[0];

                    } else if (filter.hasOwnProperty("LT")) {
                        return section[Object.keys(filter.LT)[0]] < Object.values(filter.LT)[0];

                    } else {

                        return section[Object.keys(filter.EQ)[0]] === Object.values(filter.EQ)[0];
                    }
                }
            }
        } else if (filter.hasOwnProperty("NOT")) {
            // recursively call the same function with the same inputs but negated
            return (!this.isSectionValid(filter.NOT, section));

            // Check if it is an SComparison
        } else if (filter.hasOwnProperty("IS")) {

            return InsightFacade.handleSComparisonHelper(filter.IS, section);

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
