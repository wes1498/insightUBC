import Log from "../Util";
import {IInsightFacade, InsightDataset, InsightDatasetKind} from "./IInsightFacade";
import {JSZipObject} from "jszip";

/**
 * This is the main programmatic entry point for the project.
 * Method documentation is in IInsightFacade
 *
 */

const JSZip = require("jszip");
const fs = require("fs");
const COURSES = InsightDatasetKind.Courses;
const courses: Map<string, Course[]> = new Map<string, Course[]>();

export class Course {
    [key: string]: any;
    public dept: string;
    public id: string;
    public avg: number;
    public instructor: string;
    public title: string;
    public pass: number;
    public fail: number;
    public audit: number;
    public uuid: string;
    public year: number;

    constructor (dept: string, id: string, avg: number, instructor: string, title: string, pass: number, fail: number, audit: number, uuid: string, year: number) {
        this.dept = dept;
        this.id = id;
        this.avg = avg;
        this.instructor = instructor;
        this.title = title;
        this.pass = pass;
        this.fail = fail;
        this.audit = audit;
        this.uuid = uuid;
        this.year = year;
    }
}

export interface IDataset {
    id: string;
    data: any[];
}
export default class InsightFacade implements IInsightFacade {

    private loadDatasets: Map<string, IDataset[]>;

    // public static insightFacade: InsightFacade = new InsightFacade();

    constructor() {
        Log.trace("InsightFacadeImpl::init()");
        this.loadDatasets = new Map<string, IDataset[]>();
    }
    // public static getInstance() {
    //     return InsightFacade.insightFacade;
    // }

    public addDataset(id: string, content: string, kind: InsightDatasetKind): Promise<string[]> {
        let that = this;
        return new Promise(function (resolve, reject) {
            // let isDatasetLoaded = false;
            // if (that.loadDatasets.has(id) || fs.existsSync(id).toJson()) {
            //     isDatasetLoaded = true;
            // }
            let zip = new JSZip();
            if (kind === COURSES) {
                that.coursesHelper(id, content, zip, resolve, reject);
            } else {
                reject("error occured in handling dataset");
            }
        });
    }

    public  coursesHelper(id: string, content: string, zip: any, resolve: any, reject: any) {
        zip.loadAsync(content, {base64: true}).then(function (result: any) {
            let format: IDataset = {
                id: "courses",
                data: []
            };
            let promises: Promise<any>[] = [];
            result.forEach(function (relativePath: string, file: JSZipObject) {
                if (file.dir.valueOf()) {
                    return;
                }
                let promise = file.async("text").then(function (results: any) {
                    try {
                        let Jformat = JSON.parse(results);
                        Jformat.result.forEach(function (key: any) {
                            let CourseKeys: Course = new Course(key.Subject, key.Course, key.Avg, key.Professor, key.Title, key.Pass, key.Fail, key.Audit, key.id.toString(), key.Year);
                            format.data.push(CourseKeys);
                        });
                            // fulfill(result);
                    } catch (err) {
                        reject("shieeet");
                    }
                }).catch(function (err) {
                       reject(err);
                });
                promises.push(promise);
            });
            Promise.all(promises).then(function (value: any) {
                this.loadedDataset.set("courses", format);
                resolve(value);
            }).catch(function (err: any) {
                reject(err);
            });
        }).catch(function (error: any) {
            reject(error);
        });
    }

    public removeDataset(id: string): Promise<string> {
        return Promise.reject("Not implemented.");
    }

    public performQuery(query: any): Promise <any[]> {
        return Promise.reject("Not implemented.");
    }

    public listDatasets(): Promise<InsightDataset[]> {
        return new Promise<InsightDataset[]>((fulfill, reject) => {
            const result: InsightDataset[] = [];
            for (const id of courses.keys()) {
                const numRows = courses.get(id).length;
                result.push({id, kind: COURSES, numRows});
            }
            const body: InsightDataset[] = result;
            fulfill();
        });
    }
}
