import { useEffect } from 'react';
import React, { useId, useState } from 'react';
import Image from 'next/image';
import UploadImage from '~/public/assets/image.svg';

export function FileInput(props: any) {
  const [image, setImage] = useState<any>(null);
  const handleChange = (e: any) => {
    setImage(URL.createObjectURL(e.target.files[0]));
    props.imageCompressorHandler(e.target.files[0]);

    props.setValue(props?.register.name, e.target.files[0]);
  };
  const handleDelete = (InputName: string) => {
    setImage(null);
    props.setValue(InputName, null);
  };
  useEffect(() => {
    if (typeof props?.getValues('thumb') !== 'object') {
      const linkData = `${
        process.env.NEXT_PUBLIC_CLOUD_FRONT_BASE_URL
      }${props?.getValues('thumb')}`;

      setImage(linkData.includes('undefined') ? null : linkData);
    }
  }, [props?.getValues('thumb')]);
  return (
    <div className=" relative flex items-center justify-center w-full p-2   ">
      {image !== null ? (
        <div className="border-2 border-dashed w-full p-2 border-gray-600 rounded-md">
          <div
            onClick={() => handleDelete(props?.register?.name)}
            className=" absolute top-[-16px] right-[-14px] h-10 w-10 duration bg-white  hover:bg-ac-2 hover:text-black text-black rounded-full flex justify-center items-center text-center"
          >
            <i className={` fa fa-remove text-xl `}></i>
          </div>
          <Image
            width={5000}
            height={5000}
            src={image}
            alt="uploaded Image"
            className="w-full h-64 mb-3 object-contain text-gray-400"
          />
        </div>
      ) : (
        <>
          <label
            htmlFor="dropzone-file"
            className="flex flex-col items-center justify-center bg-transparent  w-full h-64 border-2 border-dashed rounded-lg cursor-pointer      border-gray-600 "
          >
            <div className="flex flex-col items-center justify-center pt-5 pb-6">
              <i className="fas fa-image text-7xl"></i>
              {/* <Image
                width={10}
                height={10}
                src={UploadImage.src}
                alt="upload Image"
                className="w-14 h-14 mb-3 text-white bg-white p-1 rounded-md"
              ></Image>
 */}
              <p className="mb-2 text-sm text-gray-500 dark:text-gray-400">
                <span className="font-semibold">
                  {props?.placeholder ? props?.placeholder : 'Upload file '}
                </span>
              </p>
              <p className="text-xs text-gray-500 dark:text-gray-400">
                File types supported: JPG, PNG, GIF, SVG, MP4, WEBM, MP3,WAV,
                OGG, GLB
              </p>
            </div>
            <input
              id="dropzone-file"
              type="file"
              className="sr-only"
              accept="image/*"
              required
              // {...props?.register}
              onChange={(e) => handleChange(e)}
            />
          </label>
        </>
      )}
    </div>
  );
}
export function ImageInput(props: any) {
  const [image, setImage] = useState<string>('');

  useEffect(() => {
    const imgSrc = props?.getValues('thumb');
    console.log({ imgSrc });

    if (imgSrc && !imgSrc?.includes('blob:http')) {
      const linkData = `${process.env.NEXT_PUBLIC_CLOUD_FRONT_BASE_URL}${imgSrc}`;
      console.log({ linkData });
      setImage(linkData);
    }
  }, [props?.getValues('thumb')]);

  function handleChange(e: any) {
    const imageUrl = URL.createObjectURL(e.target.files[0]);

    setImage(imageUrl);
    props.onChange(e.target.files[0]);
  }

  function handleDelete() {
    setImage('');
    props.onRemove(undefined);
    props.setValue(props?.register?.name, '');
  }

  return (
    <div className=" relative flex items-center justify-center w-full">
      {image ? (
        <div className="border-2 border-dashed w-full p-2 border-gray-600 rounded-md">
          <div
            onClick={handleDelete}
            className=" absolute cursor-pointer top-[-16px] right-[-14px] h-10 w-10 duration bg-white  hover:bg-ac-2 hover:text-black text-black rounded-full flex justify-center items-center text-center"
          >
            <i className={` fa fa-remove text-xl `}></i>
          </div>
          <Image
            width={5000}
            height={5000}
            src={image}
            alt="uploaded Image"
            className="w-full h-64 mb-3 object-contain text-gray-400"
          />
        </div>
      ) : (
        <>
          <label
            htmlFor="dropzone-file"
            className="flex flex-col items-center justify-center bg-transparent  w-full h-64 border-2 border-dashed rounded-lg cursor-pointer border-gray-600 "
          >
            <div className="flex flex-col items-center justify-center pt-5 pb-6">
              <i className="fas fa-image text-7xl"></i>

              <p className="mb-2 text-sm text-gray-500 dark:text-gray-400">
                <span className="font-semibold">
                  {props?.placeholder ? props?.placeholder : 'Upload file '}
                </span>
              </p>
              <p className="text-xs text-gray-500 dark:text-gray-400">
                File types supported: JPG, PNG, GIF, SVG
              </p>
            </div>
            <input
              id="dropzone-file"
              type="file"
              className="sr-only"
              accept="image/*"
              required
              onChange={handleChange}
            />
          </label>
        </>
      )}
    </div>
  );
}

export function FileInput2(props: any) {
  const [image, setImage] = useState<any>(null);
  const handleChange = (e: any) => {
    setImage(URL.createObjectURL(e.target.files[0]));
    props.imageCompressorHandler(e.target.files[0]);

    props.setValue(props?.register.name, e.target.files[0]);
  };
  const handleDelete = (InputName: string) => {
    setImage(null);
    props?.setImageState(null);
    props.setValue('thumb', null);
  };
  useEffect(() => {
    if (typeof props?.getValues(props?.register.name) !== 'object') {
      const linkData = `${
        process.env.NEXT_PUBLIC_CLOUD_FRONT_BASE_URL
      }/${props?.getValues(props?.register.name)}`;

      setImage(linkData.includes('undefined') ? null : linkData);
    }
  }, [props?.getValues(props?.register.name)]);

  return (
    <div className=" relative flex items-center justify-center w-full p-2  ">
      {image !== null ? (
        <>
          <div
            onClick={() => handleDelete(props?.register?.name)}
            className=" absolute top-[-16px] right-[-14px] h-10 w-10 duration bg-white text-ac-2 hover:bg-ac-2 hover:text-white rounded-full flex justify-center items-center text-center"
          >
            <i className={` fas fa-remove text-xl `}></i>
          </div>
          <Image
            width={5000}
            height={5000}
            src={image}
            alt="uploaded Image"
            quality={100}
            className="w-full h-52 mb-3 text-gray-400 object-cover object-center"
          />
        </>
      ) : (
        <>
          <label
            htmlFor="dropzone-file"
            className="flex flex-col items-center justify-center bg-white  w-full h-64 border-2 border-dashed rounded-lg cursor-pointer    dark:hover:bg-bray-800 dark:bg-gray-700  dark:border-gray-600 dark:hover:border-gray-500 dark:hover:bg-gray-600"
          >
            <div className="flex flex-col items-center justify-center pt-5 pb-6">
              <Image
                width={10}
                height={10}
                src={UploadImage.src}
                alt="upload Image"
                className="w-10 h-10 mb-3 text-gray-400"
              ></Image>

              <p className="mb-2 text-sm text-gray-500 dark:text-gray-400">
                <span className="font-semibold">
                  {props?.placeholder ? props?.placeholder : 'Upload file '}
                </span>
              </p>
              <p className="text-xs text-gray-500 dark:text-gray-400">
                File types supported: JPG, PNG
              </p>
            </div>
            <input
              id="dropzone-file"
              type="file"
              className="sr-only"
              accept="image/*"
              required
              // {...props?.register}
              onChange={(e) => handleChange(e)}
            />
          </label>
        </>
      )}
    </div>
  );
}

export function MultiFileInput(props: any) {
  const [image, setImage] = useState<any>(null);
  const [files, setFiles] = useState<any>([]);

  const handleChange = (e: any) => {
    // const ImagesArray = Object.entries(e.target.files).map((data: any) =>
    //   URL.createObjectURL(data[1]),
    // );
    // console.log(ImagesArray);
    // if (ImagesArray.length > 5) {
    //   alert('You can only upload up to 5 images at a time.');
    //   return;
    // }
    // setFile([...file, ...ImagesArray]);
    // console.log('file', file);
    const files = Array.from(e.target.files);
    const imageFiles = files.filter((file: any) =>
      file.type.startsWith('image/'),
    );

    if (imageFiles.length > 8) {
      alert('You can only upload up to 5 images at a time.');
      return;
    }

    setFiles(imageFiles);
    // setImage(URL.createObjectURL(e.target.files[0]));
    props.imageCompressorHandler(files, 'multiple');

    props.setValue(props?.register.name, e.target.files);
  };
  const handleDelete = (InputName: string) => {
    setImage(null);
    props.setValue(InputName, null);
  };
  useEffect(() => {
    if (typeof props?.getValues(props?.register.name) !== 'object') {
      const linkData = `${
        process.env.NEXT_PUBLIC_CLOUD_FRONT_BASE_URL
      }${props?.getValues(props?.register.name)}`;

      setImage(linkData.includes('undefined') ? null : linkData);
    }
  }, [props?.getValues(props?.register.name)]);
  function deleteFile(e: number) {
    const newSelectedImages = files.filter((_: any, i: number) => i !== e);
    setFiles(newSelectedImages);
  }
  return (
    <div className=" relative flex items-center justify-center p-2   w-full">
      {files && files.length > 0 ? (
        <div className=" flex items-center gap-4 overflow-x-scroll lg:w-[1100px] p-4    h-64   border-2 border-dashed  border-gray-600 rounded-md">
          {files.map((item: any, index: any) => {
            return (
              <div key={item} className="relative h-full  w-1/3">
                <Image
                  width={1200}
                  className=" h-full p-2 bg-white rounded-md min-w-[200px]"
                  height={1200}
                  src={URL.createObjectURL(item)}
                  alt=""
                />
                {/* <button type="button" onClick={() => deleteFile(index)}>
                delete
              </button> */}
                <div
                  onClick={() => deleteFile(index)}
                  className=" absolute top-[-16px] right-[-14px] z-30 h-10 w-10 duration bg-white  hover:bg-ac-2 hover:text-black text-black rounded-full flex justify-center items-center text-center"
                >
                  <i className={` fa fa-remove text-xl `}></i>
                </div>
              </div>
            );
          })}
          {/* <div
            onClick={() => handleDelete(props?.register?.name)}
            className=" absolute top-[-16px] right-[-14px] h-10 w-10 duration bg-white  hover:bg-ac-2 hover:text-black text-black rounded-full flex justify-center items-center text-center"
          >
            <i className={` fa fa-remove text-xl `}></i>
          </div>
          <Image
            width={5000}
            height={5000}
            src={image}
            alt="uploaded Image"
            className="w-full h-64 mb-3 object-contain text-gray-400"
          /> */}
        </div>
      ) : (
        <>
          <label
            htmlFor="dropzone-file-1"
            className="flex flex-col items-center justify-center bg-transparent  w-full h-64 border-2 border-dashed rounded-lg cursor-pointer      border-gray-600 "
          >
            <div className="flex flex-col items-center justify-center pt-5 pb-6">
              <i className="fas fa-image text-7xl"></i>
              {/* <Image
                width={10}
                height={10}
                src={UploadImage.src}
                alt="upload Image"
                className="w-14 h-14 mb-3 text-white bg-white p-1 rounded-md"
              ></Image>
 */}
              <p className="mb-2 text-sm text-gray-500 dark:text-gray-400">
                <span className="font-semibold">
                  {props?.placeholder ? props?.placeholder : 'Upload file '}
                </span>
              </p>
              <p className="text-xs text-gray-500 dark:text-gray-400">
                File types supported: JPG, PNG, GIF, SVG, MP4, WEBM, MP3,WAV,
                OGG, GLB
              </p>
            </div>
            <input
              id="dropzone-file-1"
              type="file"
              className="sr-only"
              accept="image/*"
              disabled={files.length === 5}
              required
              // {...props?.register}
              onChange={(e) => handleChange(e)}
              multiple
            />
          </label>
        </>
      )}
    </div>
  );
}
export function SeoImage(props: any) {
  const [image, setImage] = useState<any>(null);
  const handleChange = (e: any) => {
    setImage(URL.createObjectURL(e.target.files[0]));

    props.setValue(props?.register.name, e.target.files[0]);
    props?.setOptimizeFile(e.target.files[0]);
    props?.setImageName(props?.register.name);
  };
  const handleDelete = (InputName: string) => {
    setImage(null);
    props.setValue(InputName, null);
  };
  // useEffect(() => {
  //   console.log(props?.imageState, 'props?.imageState');
  //   const linkData = `${process.env.NEXT_PUBLIC_CLOUD_FRONT_BASE_URL}/${props?.imageState}`;
  //   setImage(
  //     linkData.includes('undefined') || props?.imageState == ''
  //       ? null
  //       : linkData,
  //   );
  //   props.setValue(props?.register.name, props?.imageState);
  // }, [props?.imageState]);
  useEffect(() => {
    if (typeof props?.getValues(props?.register.name) !== 'object') {
      const linkData = `${
        process.env.NEXT_PUBLIC_CLOUD_FRONT_BASE_URL
      }/${props?.getValues(props?.register.name)}`;

      setImage(linkData.includes('undefined') ? null : linkData);
    }
  }, [props?.getValues(props?.register.name)]);
  return (
    <div
      id={props?.name}
      className=" relative flex items-center justify-center w-full p-2  "
    >
      {image !== null ? (
        <>
          <div
            onClick={() => handleDelete(props?.register?.name)}
            className=" absolute top-[-16px] right-[-14px] h-10 w-10 duration bg-white text-ac-2 hover:bg-ac-2 hover:text-white rounded-full flex justify-center items-center text-center"
          >
            <i className={` fas fa-remove text-xl `}></i>
          </div>

          <Image
            width={5000}
            height={5000}
            src={image}
            alt="uploaded Image"
            quality={100}
            className="w-full h-52 mb-3 text-gray-400 object-cover object-center"
          />
        </>
      ) : (
        <>
          <label
            htmlFor={`dropzone-file-${props?.name}`}
            className="flex flex-col items-center justify-center bg-white  w-full h-64 border-2 border-dashed rounded-lg cursor-pointer    dark:hover:bg-bray-800 dark:bg-gray-700  dark:border-gray-600 dark:hover:border-gray-500 dark:hover:bg-gray-600"
          >
            <div className="flex flex-col items-center justify-center pt-5 pb-6">
              <Image
                width={10}
                height={10}
                src={UploadImage.src}
                alt="upload Image"
                className="w-10 h-10 mb-3 text-gray-400"
              ></Image>

              <p className="mb-2 text-sm text-gray-500 dark:text-gray-400">
                <span className="font-semibold">
                  {props?.placeholder ? props?.placeholder : 'Upload file '}
                </span>
              </p>
              <p className="text-xs text-gray-500 dark:text-gray-400">
                File types supported: JPG, PNG
              </p>
            </div>
            <input
              id={`dropzone-file-${props?.name}`}
              type="file"
              className="sr-only"
              accept="image/*"
              required
              // {...props?.register}
              onChange={(e) => handleChange(e)}
            />
          </label>
        </>
      )}
    </div>
  );
}
