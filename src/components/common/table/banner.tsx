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
import { ScrollArea, ScrollBar } from '~/components/ui/scroll-area';
import { Button } from '@/ui/button';
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
import { displayDate, renderNFTImage } from '~/utils/helper';

import Link from 'next/link';
import { Switch } from '~/components/ui/switch';
import { SettingDialog } from '../modal/setting';
import { LoadingDialog } from '../modal/loadingModal';
import LanguageSelect, { LanguageInterface } from '../language_select';
import { useMemo, useState } from 'react';
import { TableFilters } from './table_filters';
import {
  Select,
  SelectContent,
  SelectItem,
  SelectTrigger,
  SelectValue,
} from '~/components/ui/select';
import {
  DoubleArrowLeftIcon,
  DoubleArrowRightIcon,
} from '@radix-ui/react-icons';
import { ChevronLeftIcon, ChevronRightIcon } from '@radix-ui/react-icons';
export default function DataTableBanner() {
  const [sorting, setSorting] = useState<SortingState>([]);
  const initialOrderFilters: any = {
    startDate: null,
    endDate: null,
    searchQuery: '',
    group: 'BANNER',

    lang_id: 1,
    rows: 10,
    first: 0,
    page: 0,
  };
  const [filterID, setFilterID] = useState({});

  const [filters, setFilters] = useState({
    ...initialOrderFilters,
  });
  const {
    data: bannerApi,
    refetch,
    isFetched,
    isLoading,
    isError,
  } = trpc.settings.get_banner.useQuery(
    { ...filters, filters: { ...filterID } },
    {
      refetchOnWindowFocus: false,

      // enabled: user?.id ? true : false,
    },
  );

  const [columnFilters, setColumnFilters] = useState<ColumnFiltersState>([]);
  const [columnVisibility, setColumnVisibility] = useState<VisibilityState>({});
  const [rowSelection, setRowSelection] = useState({});
  const [selectedItem, setSelectedItem] = useState({});
  const [title, setTitle] = useState('');
  const [type, setType] = useState('');
  const [isModal, setIsModal] = useState(false);
  const handleEnbled = (data: any, type: string) => {
    // console.log({ e, data });
    setSelectedItem(data);
    setTitle('Banner');
    setType(type);
    setIsModal(true);
  };
  const columns: ColumnDef<any>[] = [
    {
      id: 'title',
      header: 'Title',

      cell: ({ row }) => {
        const payment =
          row?.original?.value && JSON?.parse(row?.original?.value);

        return (
          <div className="flex items-center gap-2">
            <Image
              className="object-contain bg-ac-2 h-10 w-16 rounded-lg"
              src={renderNFTImage(payment)}
              alt={row?.original?.title}
              width={32}
              height={32}
            />

            <p className="w-40 text-ellipsis whitespace-nowrap overflow-hidden">
              {row?.original?.title}
            </p>
          </div>
        );
      },
    },
    {
      id: 'description',
      header: 'Description',

      cell: ({ row }) => {
        return (
          <p className="w-40 text-ellipsis whitespace-nowrap overflow-hidden">
            {row?.original?.description}
          </p>
        );
      },
    },

    {
      id: 'model',
      header: 'Model',

      cell: ({ row }) => {
        return (
          <p className="w-40 text-ellipsis whitespace-nowrap overflow-hidden">
            {row?.original?.model}
          </p>
        );
      },
    },
    {
      id: 'link',
      header: 'Link',

      cell: ({ row }) => {
        return (
          <p className="w-40 text-ellipsis whitespace-nowrap overflow-hidden">
            {row?.original?.link}
          </p>
        );
      },
    },
    {
      id: 'price',
      header: 'Price',

      cell: ({ row }) => {
        return (
          <p className="w-40 text-ellipsis whitespace-nowrap overflow-hidden">
            {row?.original?.price}
          </p>
        );
      },
    },
    {
      id: 'date',
      header: 'Date',

      cell: ({ row }) => {
        return (
          <p className="w-40 text-ellipsis whitespace-nowrap overflow-hidden">
            {row?.original?.date}
          </p>
        );
      },
    },

    {
      id: 'enabled',
      header: 'Enabled',

      cell: ({ row }) => {
        return (
          <>
            <Switch
              checked={row?.original?.is_enabled}
              onCheckedChange={() => handleEnbled(row?.original, 'enabled')}
            />
          </>
        );
      },
    },
    {
      accessorKey: 'Created At',
      header: 'Created At',
      cell: ({ row }) => (
        <div className="capitalize text-ellipsis whitespace-nowrap overflow-hidden ">
          {displayDate(row?.original?.created_at)}
        </div>
      ),
    },

    {
      id: 'actions',
      header: 'Actions',
      enableHiding: false,
      cell: ({ row }) => {
        const payment = row?.original;

        return (
          <DropdownMenu>
            <DropdownMenuTrigger asChild>
              <Button variant="ghost" className="h-8 w-8 p-0">
                <span className="sr-only">Open menu</span>
                <MoreHorizontal className="h-4 w-4" />
              </Button>
            </DropdownMenuTrigger>
            <DropdownMenuContent align="end">
              <Link href={`/admin/settings/banners/edit/${payment?.id}`}>
                <DropdownMenuItem>Edit Banner</DropdownMenuItem>
              </Link>
              <DropdownMenuItem
                onClick={() => handleEnbled(row?.original, 'delete')}
              >
                Delete Banner
              </DropdownMenuItem>
            </DropdownMenuContent>
          </DropdownMenu>
        );
      },
    },
  ];

  const bannerData = useMemo(() => {
    return Array.isArray(bannerApi?.data) ? bannerApi?.data ?? [] : [];
  }, [bannerApi]);

  const table = useReactTable({
    data: bannerData,
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

  function languageHandler(params: LanguageInterface) {
    setFilters((prevFilters: any) => ({
      ...prevFilters,
      lang_id: params.id,
    }));
  }
  function handlePagination(page: number) {
    if (page < 0) return;
    setFilters((prevFilters: any) => ({ ...prevFilters, first: page }));
  }
  const roleOptions1 = [
    {
      Icon: 'fal fa-chevron-down',
      text: 'Search',
      filtername: 'searchQuery',
      type: 'text',
    },
    {
      Icon: 'fal fa-chevron-down',
      text: 'Enabled',
      filtername: 'is_enabled',
      type: 'select',

      filter: [
        {
          name: 'Yes',
          value: true,
        },
        {
          name: 'No',
          value: false,
        },
      ],
    },

    {
      Icon: 'fal fa-chevron-down',
      text: 'From Date',
      filtername: 'startDate',
      type: 'date',
    },
    {
      Icon: 'fal fa-chevron-down',
      text: 'To Date',
      filtername: 'endDate',
      type: 'date',
    },
    {
      Icon: 'fal fa-chevron-down',
      text: 'Clear Filter',
      filtername: 'Clear',
    },
  ];
  return (
    <div className="w-full">
      <div className="flex justify-between items-center py-4">
        <div></div>
        <div className="flex gap-2">
          <LanguageSelect languageHandler={languageHandler} />

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
          <TableFilters
            inputList={roleOptions1}
            item_name={'Banner'}
            value={filterID}
            setValue={setFilterID}
            setFilters={setFilters}
            initial={initialOrderFilters}
          />
        </div>
      </div>
      <div className="rounded-md border border-border">
        <ScrollArea className="w-full ">
          <ScrollBar orientation="horizontal"></ScrollBar>
          <Table className="w-[90vw] md:w-full">
            <TableHeader className="bg-secondary/80">
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
                    dir={filters?.lang_id == 1 ? 'ltr' : 'rtl'}
                  >
                    {row.getVisibleCells().map((cell) => (
                      <TableCell key={cell.id}>
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
        </ScrollArea>
      </div>
      <div className="flex items-center justify-end space-x-2 py-4">
        <div className="flex-1 flex w-[100px] items-center justify-start text-sm font-medium">
          Page {filters.first + 1} of{' '}
          {Math.ceil((bannerApi?.count ?? 0) / filters.rows)}
        </div>

        <div className="flex items-center justify-center space-x-6 lg:space-x-8">
          <div className="flex items-center space-x-2">
            <p className="text-sm font-medium">Rows per page</p>
            <Select
              value={`${filters.rows}`}
              onValueChange={(value) => {
                setFilters((prevFilters: any) => ({
                  ...prevFilters,
                  rows: Number(value),
                  first: 0,
                }));
                table.setPageSize(Number(value));
              }}
            >
              <SelectTrigger className="h-8 w-[70px]">
                <SelectValue placeholder={filters.rows} />
              </SelectTrigger>
              <SelectContent side="top">
                {[5, 10, 20, 30, 40, 50].map((pageSize) => (
                  <SelectItem key={pageSize} value={`${pageSize}`}>
                    {pageSize}
                  </SelectItem>
                ))}
              </SelectContent>
            </Select>
          </div>

          <div className="flex items-center space-x-2">
            <Button
              variant="outline"
              className="hidden h-8 w-8 p-0 lg:flex"
              onClick={() => handlePagination(0)}
              disabled={filters.first === 0}
            >
              <span className="sr-only">Go to first page</span>
              <DoubleArrowLeftIcon className="h-4 w-4" />
            </Button>

            <Button
              variant="outline"
              className="h-8 w-8 p-0"
              onClick={() => handlePagination(filters?.first - 1)}
              disabled={filters?.first === 0}
            >
              <span className="sr-only">Go to previous page</span>
              <ChevronLeftIcon className="h-4 w-4" />
            </Button>
            <Button
              variant="outline"
              className="h-8 w-8 p-0"
              onClick={() => handlePagination(filters.first + 1)}
              disabled={
                (filters.first + 1) * filters.rows > (bannerApi?.count ?? 0) ||
                Math.ceil((bannerApi?.count ?? 0) / filters.rows) ==
                  filters.first + 1
              }
            >
              <span className="sr-only">Go to next page</span>
              <ChevronRightIcon className="h-4 w-4" />
            </Button>

            <Button
              variant="outline"
              className="hidden h-8 w-8 p-0 lg:flex"
              onClick={() =>
                handlePagination(
                  Math.ceil((bannerApi?.count ?? 0) / filters.rows) - 1,
                )
              }
              disabled={
                (filters.first + 1) * filters.rows > (bannerApi?.count ?? 0) ||
                Math.ceil((bannerApi?.count ?? 0) / filters.rows) ==
                  filters.first + 1
              }
            >
              <span className="sr-only">Go to last page</span>
              <DoubleArrowRightIcon className="h-4 w-4" />
            </Button>
          </div>
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
        type={type}
        setType={setType}
      />
      <LoadingDialog open={isLoading} text={'Loading data...'} />
    </div>
  );
}
