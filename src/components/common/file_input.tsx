import { useEffect } from 'react';
import React, { useState } from 'react';
import Image from 'next/image';

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
        process.env.NEXT_PUBLIC_MEDIA_BASE_URL
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
                File types supported: JPG, PNG, SVG
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
      const linkData = `${process.env.NEXT_PUBLIC_MEDIA_BASE_URL}${imgSrc}`;
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
                File types supported: JPG, PNG, SVG
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

type EventImageType = {
  id: number;
  thumb: string;
  event_id: number;
};

export function MultiFileInput(props: any) {
  const [images, setImages] = useState<string[]>([]);

  useEffect(() => {
    if (props?.eventImages?.length) {
      const eventImages = props?.eventImages?.map(
        (image: any) =>
          `${process.env.NEXT_PUBLIC_MEDIA_BASE_URL}${image.thumb}`,
      );
      setImages(eventImages);
    }
  }, [props?.eventImages]);

  function handleChange(e: React.ChangeEvent<HTMLInputElement>) {
    if (e.target.files) {
      const newFiles = Array.from(e.target.files);
      const imageFiles = newFiles.filter((file: File) =>
        file.type.startsWith('image/'),
      );

      if ([...props?.files, ...imageFiles].length >= 15) {
        alert('You can only upload up to 15 images at a time.');
        return;
      }

      const viewImages = imageFiles.map((image: any) =>
        URL.createObjectURL(image),
      );

      setImages((prevImages) => [...prevImages, ...viewImages]);
      props.imageCompressorHandler(imageFiles, 'multiple');
    }
  }

  function deleteFile(url: string, index: number) {
    const newSelectedImages = images.filter((_: any, i: number) => i !== index);
    const newSelectedFiles = props?.files.filter(
      (_: any, i: number) => i !== index,
    );

    setImages(newSelectedImages);
    props?.setFiles(newSelectedFiles);

    if (url.includes('upload/') && props?.eventImages?.length) {
      const uploadedImage: EventImageType = props?.eventImages?.find(
        (item: EventImageType) =>
          process.env.NEXT_PUBLIC_MEDIA_BASE_URL + item.thumb === url,
      );

      props?.setRemovedImages((prevImages: EventImageType[]) => [
        ...prevImages,
        uploadedImage,
      ]);
    }
  }

  return (
    <div className=" relative flex items-center justify-center p-2 flex-1">
      {images && images.length > 0 ? (
        <div className="space-y-3">
          <div className="w-[70vw] overflow-x-scroll p-4  flex items-center gap-4 border-2 border-dashed  border-gray-600 rounded-md">
            {images.map((item: string, index: number) => {
              return (
                <div
                  key={item}
                  className="relative h-56 min-w-[280px] w-1/3 max-w-xs"
                >
                  <Image
                    width={1200}
                    className=" h-full p-2 bg-white rounded-md w-full"
                    height={1200}
                    src={item}
                    alt=""
                  />

                  <div
                    onClick={() => deleteFile(item, index)}
                    className="cursor-pointer absolute top-[-16px] right-[-14px] z-30 h-10 w-10 duration bg-white  hover:bg-ac-2 hover:text-black text-black rounded-full flex justify-center items-center text-center"
                  >
                    <i className={` fa fa-remove text-xl `}></i>
                  </div>
                </div>
              );
            })}
          </div>

          <label
            htmlFor="dropzone-file-1"
            className="bg-background border border-border block ml-auto py-2 px-3 w-max cursor-pointer"
          >
            Add more
            <input
              id="dropzone-file-1"
              type="file"
              className="sr-only"
              accept="image/*"
              disabled={props?.files?.length >= 15}
              onChange={handleChange}
              multiple
            />
          </label>
        </div>
      ) : (
        <>
          <label
            htmlFor="dropzone-file-1"
            className="flex flex-col items-center justify-center bg-transparent flex-1 h-64 border-2 border-dashed rounded-lg cursor-pointer      border-gray-600 "
          >
            <div className="flex flex-col items-center justify-center pt-5 pb-6">
              <i className="fas fa-image text-7xl"></i>

              <p className="mb-2 text-sm text-gray-500 dark:text-gray-400">
                <span className="font-semibold">
                  {props?.placeholder ? props?.placeholder : 'Upload file '}
                </span>
              </p>
              <p className="text-xs text-gray-500 dark:text-gray-400">
                File types supported: JPG, PNG, SVG
              </p>
            </div>
            <input
              id="dropzone-file-1"
              type="file"
              className="sr-only"
              accept="image/*"
              disabled={props?.files?.length >= 15}
              required
              onChange={handleChange}
              multiple
            />
          </label>
        </>
      )}
    </div>
  );
}
