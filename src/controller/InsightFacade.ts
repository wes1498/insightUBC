import Log from "../Util";
import {
IInsightFacade,
    InsightCourse,
    InsightDataset,
    InsightDatasetKind,
    InsightError,
    NotFoundError,
    InsightFilter
} from "./IInsightFacade";
import * as JSZip from "jszip";
import {JSZipObject} from "jszip";
import * as fs from "fs";
import {fileExists} from "ts-node";
import {isNumber, isString} from "util";

/**
 * This is the main programmatic entry point for the project.
 * Method documentation is in IInsightFacade
 *
 */
export default class InsightFacade implements IInsightFacade {

    private coursesMap: Map<string, object[]>;

    constructor() {
        Log.trace("InsightFacadeImpl::init()");
        this.coursesMap = new Map<string, object[]>();
    }
    private readFiles(dirname: string): void {

        fs.readdir(dirname, (err, filenames) => {
            if (err) {
                return;
            }
            filenames.forEach((filename) => {
                if (!this.coursesMap.has(filename)) {
                    fs.readFile (dirname + filename , "utf-8", (err2, content: string) => {

                        if (err2) {
                            return;
                        }
                        this.coursesMap.set(filename, JSON.parse(content));
                    });
                }

            });
        });
    }
    private something() {
        this.pathReader("data/");
    }
    public addDataset(id: string, content: string, kind: InsightDatasetKind): Promise<string[]> {
        return new Promise<string[]>((resolve, reject) => {
            this.something();
            if (!id || id.length === 0 || this.coursesMap.has(id)) {
                return reject(new InsightError("Invalid Id"));
            } else {
                this.coursesMap.set(id, []);
            }
            JSZip.loadAsync(content, {base64: true}).then((decoded: JSZip) => {
                let desiredFolder: string = kind === InsightDatasetKind.Courses ? "courses/" : "rooms/";

                if (decoded.length === 0) {
                    return reject(new InsightError("No valid data in the zip file"));
                } else if (!decoded.files.hasOwnProperty(desiredFolder)) {
                    return reject(new InsightError("Desired folder for the dataset kind does not exist"));
                }
                let filesToLoadPromise: any[] = [];
                decoded.forEach(((relativePath, fileObject: JSZipObject) => {
                    // if not folder
                    if (!fileObject.dir) {
                        filesToLoadPromise.push(fileObject.async("text").then((data: string) => {
                            this.addCourse(data, id); // save the course info a
                        }).catch((e) => {
                            return reject(new InsightError("Error processing encoded course data " + e));
                        }));
                    }
                }));

                Promise.all(filesToLoadPromise).then(() => {
                    let toSaveOnDisk: object[] = this.coursesMap.get(id);
                    if (toSaveOnDisk.length === 0) {
                        this.coursesMap.delete(id);
                        return reject(new InsightError("No sections were added"));
                    } else {
                        fs.writeFile("data/" + id, JSON.stringify(toSaveOnDisk), (e) => {
                            return reject(new InsightError("Error Saving Files"));
                        });

                        let result: string[] = [];
                        this.coursesMap.forEach((value, key, map) => {
                            result.push(key);
                        });
                        return resolve(result);
                    }
                });

            }).catch((e) => {
                return reject(new InsightError("Error decoding contents: Invalid Zip " + e));
            });

        });
    }
    public addDataset4(id: string, content: string, kind: InsightDatasetKind): Promise<string[]> {
        this.pathReader("data/"); // We are readingPaths and storing in coursesMap
        return new Promise<string[]> ( (resolve, reject) => {
            if (this.coursesMap.has(id) || id.length === 0) {
                return reject(new InsightError("ID cannot be added(Invalid/Already added)"));
            }
            this.coursesMap.set(id, []);
            JSZip.loadAsync(content, {base64: true}).then((unzipped: JSZip) => { // JSzip used to Unzip and read
                if (unzipped.length < 1) {
                    return reject(new InsightError("Unzipped file is empty"));
                }
                let loadingContents: any[] = [];
                this.addContents(unzipped, loadingContents, id);
                Promise.all(loadingContents).then(() => {
                    let toSaveOnDisk: object[] = this.coursesMap.get(id);
                    fs.writeFile("data/" + id, JSON.stringify(toSaveOnDisk), (e) => {
                            return reject(new InsightError("Error Saving Files"));
                        });

                    let result: string[] = [];
                    this.coursesMap.forEach((value, key, map) => {
                            result.push(key);
                        });
                    return resolve(result);
                });
            }).catch ((e) => {
                return reject(new InsightError("Error decoding contents: Invalid Zip " + e));
            });
        });
    }
    private addContents(unzipped: JSZip, loadingContents: any[], id: string) {  // my own method
        unzipped.forEach( ((relativePath, fileObject: JSZipObject) => {
            // if not folder
            if (!fileObject.dir) {
                loadingContents.push(fileObject.async("text").then((fileData: string) => {
                    this.addCourse(fileData, id); // save the course info
                }).catch((e) => {
                    return e;
                }));
            }
        }));
    }
    private addCourse(course: string, datasetId: string) {
        try {
            const courseInfo: object[] = JSON.parse(course).result;
            let info: any;
            for (info of courseInfo ) {
                let courseDep: string = info.Subject as string;
                let courseId: string = info.Course as string;
                let courseAvg: number = info.Avg as number;
                let courseInstru: string = info.Professor as string;
                let courseTitle: string = info.Title as string;
                let coursePass: number = info.Pass as number;
                let courseFail: number = info.Fail as number;
                let courseAu: number = info.Audit as number;
                let courseUuid: string = info.id.toString() as string;
                let courseYear: number = Number(info.Year);

                let completeCourse: InsightCourse = {
                    coursesDept: courseDep,
                    coursesId: courseId,
                    coursesAvg: courseAvg,
                    coursesInstructor: courseInstru,
                    coursesTitle: courseTitle,
                    coursesPass: coursePass,
                    coursesFail: courseFail,
                    coursesAudit: courseAu,
                    coursesUuid: courseUuid,
                    coursesYear: courseYear
                };
                for (let value of Object.values(completeCourse)) {
                    if (value === undefined || typeof value === "object" || value instanceof Array) {
                        throw new TypeError("Some of the fields are missing or of non-cast type");
                    }
                }
                this.coursesMap.get(datasetId).push(completeCourse);
            }

        } catch (e) {
            // not in JSON format or some fields of different type/missing-> skip this course
        }
    }
    private pathReader(folderName: string): void { // deprecated
        fs.readdir(folderName, (e , files) => {
            if (e) {
                let error1 =  "Error reading dir";
                return error1;
            } else {
                files.forEach((filesId) => {
                    if (this.coursesMap.has(filesId)) {
                        return;
                    } else {
                        let name = folderName + filesId;
                        fs.readFile(name, "utf-8", (e2, content: string) => {
                            if (e2) {
                                return;
                            }
                            this.coursesMap.set(filesId, JSON.parse(content));
                        });
                    }
                });
            }
        });
    }
    private pathReader2(folderName: string): void {  // this one made myself
        fs.readdir(folderName, (e , files) => {
            if (e) {
                let error1 =  "Error reading dir";
                return error1;
            } else {
                files.forEach((filesId) => {
                        let name = folderName + filesId;
                        if (fileExists(name)) {
                            return;
                        }
                        fs.readFile(name, "utf-8", (e2, content: string) => {
                            if (e2) {
                                return;
                            }
                            this.coursesMap.set(filesId, JSON.parse(content));
                        });
                });
            }
        });
    }
    public removeDataset2(id: string): Promise<string> {// no changes needed
        return new Promise<string>( (resolve, reject) => {
            if (id === null || id === "") {
                return reject(new InsightError("Invalid ID"));
            }
            // console.log("after1 " + id);
            fs.unlink("data/" + id, (err) => {
                if (err) {
                    if (this.coursesMap.has(id)) {
                        this.coursesMap.delete(id);
                        return resolve(id);
                    } else {
                        return reject(new NotFoundError("Dataset has not yet been loaded"));
                    }
                }
               // return reject(new NotFoundError("Could not find the ID, cant delete"));
            });
        });
    }
    public removeDataset(id: string): Promise<string> {
        return new Promise<string>((resolve, reject) => {
            if ( id === null || !id || id === "") {
                return reject(new InsightError("Invalid ID"));
            }

            // if(!this.courseMap.has(id)) return reject(new NotFoundError("Dataset has not yet been loaded"));

            fs.unlink("data/" + id, (err) => {
                 if (err) {
                    return reject(new NotFoundError("Dataset has not yet been loaded"));
                } else if (this.coursesMap.has(id)) {
                    this.coursesMap.delete(id);
                    return resolve(id);
                }
            });
        });
    }
    public performQuery(query: any): Promise <any[]> {// re-code
           return Promise.reject("Not implemented");
    }
    private filterDataset(dataset: any[], filter: InsightFilter, id: string): any[] {// re code
        const filteredDataset: any[] = [];
        for (const data of dataset) {
            if (this.isFilterSatisfied(filter, data, id)) {
                filteredDataset.push(data);
            }
        }
        if (!filteredDataset) {
            throw  new Error("No satisfied data found.");
        } else {
            return filteredDataset;
        }
    }
    private isFilterSatisfied(filter: InsightFilter, data: any, cid: string): boolean {// re code
        if (Object.keys(filter).length > 1) {
            throw Error("Query is malformed");
        } else if (filter.GT) {
            const key = Object.keys(filter.GT)[0];
            const val = filter.GT[key];
            const col = key.split("_")[1];
            const fid = key.split("_")[0];
            const value = data[col];
            if (cid !== fid) {
                throw new Error("Query is trying to compare two datasets at the same time.");
            }
            if (isNumber(val) && isNumber(value)) {
                return (value > val);
            } else {
                throw new Error("Invalid query: GT value should be a number.");
            }
        } else if (filter.EQ) {
            const key = Object.keys(filter.EQ)[0];
            const val = filter.EQ[key];
            const col = key.split("_")[1];
            const fid = key.split("_")[0];
            const value = data[col];
            if (cid !== fid) {
                throw new Error("Query is trying to query tow datasets a the same time");
            }
            if (isNumber(val) && isNumber(value)) {
                return (value === val);
            } else {
                throw new Error("Invalid query: EQ value should be a number.");
            }
        } else if (filter.LT) {
            const key = Object.keys(filter.LT)[0];
            const val = filter.LT[key];
            const col = key.split("_")[1];
            const fid = key.split("_")[0];
            const value: number = data[col];
            if (cid !== fid) {
                throw new Error("Query is trying to query tow datasets a the same time");
            }
            if (isNumber(val) && isNumber(value)) {
                return (value < val);
            } else {
                throw new Error("Invalid query: LT value should be a number.");
            }
        } else if (filter.IS) {
            const key = Object.keys(filter.IS)[0];
            const val: string = filter.IS[key];
            const col = key.split("_")[1];
            const fid = key.split("_")[0];
            const value: string = data[col];
            const len = val.length;
            if (cid !== fid) {
                throw new Error("Query is trying to query tow datasets a the same time");
            }
            if (isString(val) && isString(value)) {
                if (val === "*") {
                    return true;
                } else if (len === 1) {
                    return (val === value);
                } else if (val[0] === "*" && val[len - 1] === "*") {
                    return value.includes(val.substr(1, len - 2));
                } else if (val[0] === "*") {
                    return value.endsWith(val.slice(1));
                } else if (val[len - 1] === "*") {
                    return value.startsWith(val.substring(0, len - 1));
                } else if (val.includes("*")) {
                    throw new Error("Invalid query: * should not be in middle.");
                } else {
                    return (val === value);
                }
            } else {
                throw new Error("Invalid query: IS value should be a string.");
            }
        } else if (filter.NOT) {
            return (!(this.isFilterSatisfied(filter.NOT, data, cid)));
        } else if (filter.AND) {
            if (filter.AND.length === 0) {
                // Log.trace(filter.AND.length.toString());
                throw new Error("Invalid query: AND should contain at least one condition");
            }
            let res = true;
            for (const r of filter.AND) {
                if (this.isFilterSatisfied(r, data, cid) === false) {
                    res = false;
                }
            }
            return res;
        } else if (filter.OR) {
            if (filter.OR.length === 0) {
                throw new Error("Invalid query: OR should contain at least one condition");
            }
            let res = false;
            for (const r of filter.OR) {
                if (this.isFilterSatisfied(r, data, cid) === true) {
                    res = true;
                }
            }
            return res;
        } else {
            throw new Error("Invalid Query.");
        }
    }
    public listDatasets(): Promise<InsightDataset[]> { // did myself
        return new Promise<InsightDataset[]> ( (resolve, reject) => {
            let result: InsightDataset[] = [];

            for (let id of this.coursesMap.keys()) {
                let crows: number = this.coursesMap.get(id).length;
                result.push({id, kind: InsightDatasetKind.Courses, numRows: crows});
            }

            resolve(result);
        });
    }
}
