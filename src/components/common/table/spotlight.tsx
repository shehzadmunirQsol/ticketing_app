import * as React from 'react';
import {
  ColumnDef,
  ColumnFiltersState,
  SortingState,
  VisibilityState,
  flexRender,
  getCoreRowModel,
  getFilteredRowModel,
  getPaginationRowModel,
  getSortedRowModel,
  useReactTable,
} from '@tanstack/react-table';
import { ChevronDown, MoreHorizontal } from 'lucide-react';

import { Button } from '@/ui/button';
import { Checkbox } from '@/ui/checkbox';
import {
  DropdownMenu,
  DropdownMenuCheckboxItem,
  DropdownMenuContent,
  DropdownMenuItem,
  DropdownMenuLabel,
  DropdownMenuSeparator,
  DropdownMenuTrigger,
} from '@/ui/dropdown-menu';
import {
  Table,
  TableBody,
  TableCell,
  TableHead,
  TableHeader,
  TableRow,
} from '@/ui/table';
import { trpc } from '~/utils/trpc';
import Image from 'next/image';
import { renderNFTImage } from '~/utils/helper';
import {
  Select,
  SelectContent,
  SelectGroup,
  SelectItem,
  SelectTrigger,
  SelectValue,
} from '../../ui/select';
import Link from 'next/link';
import { Switch } from '~/components/ui/switch';
import { SettingDialog } from '../modal/setting';

// export const columns: ColumnDef<any>[] = [
//   {
//     id: 'select',
//     header: ({ table }) => (
//       <Checkbox
//         checked={table.getIsAllPageRowsSelected()}
//         onCheckedChange={(value: any) =>
//           table.toggleAllPageRowsSelected(!!value)
//         }
//         aria-label="Select all"
//       />
//     ),
//     cell: ({ row }) => (
//       <Checkbox
//         checked={row.getIsSelected()}
//         onCheckedChange={(value) => row.toggleSelected(!!value)}
//         aria-label="Select row"
//       />
//     ),
//     enableSorting: false,
//     enableHiding: false,
//   },

//   {
//     id: 'title',
//     header: 'Title',

//     cell: ({ row }) => {
//       const payment = row?.original?.value && JSON?.parse(row?.original?.value);

//       return (
//         <>
//           <div className="flex items-center space-x-2">
//             <Image
//               className="object-cover bg-ac-2   h-10 w-10 rounded-lg"
//               src={renderNFTImage(payment)}
//               alt={row?.original?.name}
//               width={32}
//               height={32}
//             />

//             <p className=" ">
//               {/* {customTruncateHandler(payment?.title, 15)} */}
//               {row?.original?.name}
//             </p>
//             {/* <p>{nft?.name}</p> */}
//           </div>
//         </>
//       );
//     },
//   },
//   {
//     id: 'description',
//     header: 'Description',

//     cell: ({ row }) => {
//       const payment = row?.original?.value && JSON?.parse(row?.original?.value);

//       return <>{payment?.description}</>;
//     },
//   },

//   {
//     id: 'link',
//     header: 'Link',

//     cell: ({ row }) => {
//       return <>{row?.original?.link}</>;
//     },
//   },
//   {
//     id: 'is_enabled',
//     header: 'Enabled',

//     cell: ({ row }) => {
//       return (
//         <>
//           <Switch checked={row?.original?.is_enabled} />
//         </>
//       );
//     },
//   },

//   {
//     id: 'actions',
//     enableHiding: false,
//     cell: ({ row }) => {
//       const payment = row?.original;
//       console.log(row?.original, 'row?.original');

//       return (
//         <DropdownMenu>
//           <DropdownMenuTrigger asChild>
//             <Button variant="ghost" className="h-8 w-8 p-0 ">
//               <span className="sr-only">Open menu</span>
//               <MoreHorizontal className="h-4 w-4" />
//             </Button>
//           </DropdownMenuTrigger>
//           <DropdownMenuContent align="end">
//             <DropdownMenuLabel>Actions</DropdownMenuLabel>

//             <DropdownMenuSeparator />
//             <Link href={`/admin/settings/spotlight/edit/${payment?.id}`}>
//               <DropdownMenuItem>Edit Spot Light</DropdownMenuItem>
//             </Link>
//           </DropdownMenuContent>
//         </DropdownMenu>
//       );
//     },
//   },
// ];

export default function DataTableSpotLight() {
  const initialOrderFilters: any = {
    startDate: null,
    endDate: null,
    searchQuery: '',
    group: 'WONDER',
    lang_id: 1,

    rows: 10,
    first: 0,
    page: 0,
  };
  const [orderFilters, setOrderFilters] = React.useState({
    ...initialOrderFilters,
  });
  const {
    data: BannerApiData,
    refetch,
    isFetched,
    isLoading,
    isError,
  } = trpc.settings.get_banner.useQuery(orderFilters, {
    refetchOnWindowFocus: false,

    // enabled: user?.id ? true : false,
  });
  const [sorting, setSorting] = React.useState<SortingState>([]);

  const [columnFilters, setColumnFilters] = React.useState<ColumnFiltersState>(
    [],
  );
  const [columnVisibility, setColumnVisibility] =
    React.useState<VisibilityState>({});
  const [rowSelection, setRowSelection] = React.useState({});
  const [selectedItem, setSelectedItem] = React.useState({});
  const [title, setTitle] = React.useState('');
  const [isModal, setIsModal] = React.useState(false);
  const handleEnbled = (e: boolean, data: any) => {
    console.log({ e, data });
    setSelectedItem(data);
    setTitle('Spot Light');
    setIsModal(true);
  };
  const columns: ColumnDef<any>[] = [
    {
      id: 'select',
      header: ({ table }) => (
        <Checkbox
          checked={table.getIsAllPageRowsSelected()}
          onCheckedChange={(value: any) =>
            table.toggleAllPageRowsSelected(!!value)
          }
          aria-label="Select all"
        />
      ),
      cell: ({ row }) => (
        <Checkbox
          checked={row.getIsSelected()}
          onCheckedChange={(value) => row.toggleSelected(!!value)}
          aria-label="Select row"
        />
      ),
      enableSorting: false,
      enableHiding: false,
    },

    {
      id: 'title',
      header: 'Title',

      cell: ({ row }) => {
        const payment =
          row?.original?.value && JSON?.parse(row?.original?.value);

        return (
          <>
            <div className="flex items-center space-x-2">
              <Image
                className="object-cover bg-ac-2   h-10 w-10 rounded-lg"
                src={renderNFTImage(payment)}
                alt={row?.original?.name}
                width={32}
                height={32}
              />

              <p className=" ">
                {/* {customTruncateHandler(payment?.title, 15)} */}
                {row?.original?.name}
              </p>
              {/* <p>{nft?.name}</p> */}
            </div>
          </>
        );
      },
    },
    {
      id: 'description',
      header: 'Description',

      cell: ({ row }) => {
        const payment =
          row?.original?.value && JSON?.parse(row?.original?.value);

        return <>{payment?.description}</>;
      },
    },

    {
      id: 'link',
      header: 'Link',

      cell: ({ row }) => {
        return <>{row?.original?.link}</>;
      },
    },
    {
      id: 'is_enabled',
      header: 'Enabled',

      cell: ({ row }) => {
        return (
          <>
            <Switch
              checked={row?.original?.is_enabled}
              onCheckedChange={(e) => handleEnbled(e, row?.original)}
            />
          </>
        );
      },
    },

    {
      id: 'actions',
      enableHiding: false,
      cell: ({ row }) => {
        const payment = row?.original;
        console.log(row?.original, 'row?.original');

        return (
          <DropdownMenu>
            <DropdownMenuTrigger asChild>
              <Button variant="ghost" className="h-8 w-8 p-0 ">
                <span className="sr-only">Open menu</span>
                <MoreHorizontal className="h-4 w-4" />
              </Button>
            </DropdownMenuTrigger>
            <DropdownMenuContent align="end">
              <DropdownMenuLabel>Actions</DropdownMenuLabel>

              <DropdownMenuSeparator />
              <Link href={`/admin/settings/spotlight/edit/${payment?.id}`}>
                <DropdownMenuItem>Edit Spot Light</DropdownMenuItem>
              </Link>
            </DropdownMenuContent>
          </DropdownMenu>
        );
      },
    },
  ];
  const table = useReactTable({
    data:
      BannerApiData !== undefined && BannerApiData && isFetched && !isError
        ? BannerApiData
        : [],
    columns,
    onSortingChange: setSorting,
    onColumnFiltersChange: setColumnFilters,
    getCoreRowModel: getCoreRowModel(),
    getPaginationRowModel: getPaginationRowModel(),
    getSortedRowModel: getSortedRowModel(),
    getFilteredRowModel: getFilteredRowModel(),
    onColumnVisibilityChange: setColumnVisibility,
    onRowSelectionChange: setRowSelection,
    state: {
      sorting,
      columnFilters,
      columnVisibility,
      rowSelection,
    },
  });
  function toggleLanguageHandler(lang: 'en' | 'ar') {
    setOrderFilters((prevFilters: any) => ({
      ...prevFilters,
      lang_id: lang === 'ar' ? 2 : 1,
    }));
  }

  return (
    <div className="w-full  ">
      <div className="flex justify-between items-center py-4">
        <div></div>
        <div className="flex gap-2">
          <Select onValueChange={toggleLanguageHandler}>
            <SelectTrigger className="h-10 px-4 py-2 rounded-none ">
              <SelectValue placeholder="EN" />
            </SelectTrigger>
            <SelectContent>
              <SelectGroup>
                <SelectItem value="en">EN</SelectItem>
                <SelectItem value="ar">AR</SelectItem>
              </SelectGroup>
            </SelectContent>
          </Select>
          <DropdownMenu>
            <DropdownMenuTrigger asChild>
              <Button variant="outline" className="ml-auto">
                Columns <ChevronDown className="ml-2 h-4 w-4" />
              </Button>
            </DropdownMenuTrigger>
            <DropdownMenuContent align="end">
              {table
                .getAllColumns()
                .filter((column) => column.getCanHide())
                .map((column) => {
                  return (
                    <DropdownMenuCheckboxItem
                      key={column.id}
                      className="capitalize"
                      checked={column.getIsVisible()}
                      onCheckedChange={(value) =>
                        column.toggleVisibility(!!value)
                      }
                    >
                      {column.id}
                    </DropdownMenuCheckboxItem>
                  );
                })}
            </DropdownMenuContent>
          </DropdownMenu>
        </div>
      </div>
      <div className="rounded-md border">
        <Table>
          <TableHeader>
            {table.getHeaderGroups().map((headerGroup) => (
              <TableRow key={headerGroup.id}>
                {headerGroup.headers.map((header) => {
                  return (
                    <TableHead key={header.id}>
                      {header.isPlaceholder
                        ? null
                        : flexRender(
                            header.column.columnDef.header,
                            header.getContext(),
                          )}
                    </TableHead>
                  );
                })}
              </TableRow>
            ))}
          </TableHeader>
          <TableBody>
            {table.getRowModel().rows?.length ? (
              table.getRowModel().rows.map((row) => (
                <TableRow
                  key={row.id}
                  data-state={row.getIsSelected() && 'selected'}
                  className=""
                >
                  {row.getVisibleCells().map((cell) => (
                    <TableCell key={cell.id} className=" p-6">
                      {flexRender(
                        cell.column.columnDef.cell,
                        cell.getContext(),
                      )}
                    </TableCell>
                  ))}
                </TableRow>
              ))
            ) : (
              <TableRow>
                <TableCell
                  colSpan={columns.length}
                  className="h-24 text-center"
                >
                  No results.
                </TableCell>
              </TableRow>
            )}
          </TableBody>
        </Table>
      </div>
      <div className="flex items-center justify-end space-x-2 py-4">
        <div className="flex-1 text-sm text-muted-foreground">
          {table.getFilteredSelectedRowModel().rows.length} of{' '}
          {table.getFilteredRowModel().rows.length} row(s) selected.
        </div>
        <div className="space-x-2">
          <Button
            variant="outline"
            onClick={() => table.previousPage()}
            disabled={!table.getCanPreviousPage()}
          >
            Previous
          </Button>
          <Button
            variant="outline"
            onClick={() => table.nextPage()}
            disabled={!table.getCanNextPage()}
          >
            Next
          </Button>
        </div>
      </div>
      <SettingDialog
        selectedItem={selectedItem}
        setSelectedItem={setSelectedItem}
        title={title}
        setTitle={setTitle}
        isModal={isModal}
        setIsModal={setIsModal}
        refetch={refetch}
      />
    </div>
  );
}
